{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The simplification engine is only willing to hoist allocations
-- out of loops if the memory block resulting from the allocation is
-- dead at the end of the loop.  If it is not, we may cause data
-- hazards.
--
-- This module rewrites loops with memory block merge parameters such
-- that each memory block is copied at the end of the iteration, thus
-- ensuring that any allocation inside the loop is dead at the end of
-- the loop.  This is only possible for allocations whose size is
-- loop-invariant, although the initial size may differ from the size
-- produced by the loop result.
--
-- Additionally, inside parallel kernels we also copy the initial
-- value.  This has the effect of making the memory block returned by
-- the array non-existential, which is important for later memory
-- expansion to work.
module Futhark.Optimise.DoubleBuffer
       ( doubleBuffer )
       where

import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Maybe
import           Data.List

import           Futhark.Construct
import           Futhark.Representation.AST
import           Futhark.Pass.ExplicitAllocations (arraySizeInBytesExp)
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import           Futhark.Representation.ExplicitMemory
                 hiding (Prog, Body, Stm, Pattern, PatElem,
                         BasicOp, Exp, Lambda, FunDef, FParam, LParam, RetType)
import           Futhark.Pass
import           Futhark.Util (maybeHead)

doubleBuffer :: Pass ExplicitMemory ExplicitMemory
doubleBuffer =
  Pass { passName = "Double buffer"
       , passDescription = "Perform double buffering for merge parameters of sequential loops."
       , passFunction = intraproceduralTransformation optimiseFunDef
       }

optimiseFunDef :: FunDef ExplicitMemory -> PassM (FunDef ExplicitMemory)
optimiseFunDef fundec = modifyNameSource $ \src ->
  let m = runDoubleBufferM $ inScopeOf fundec $ optimiseBody $ funDefBody fundec
      (body', src') = runState (runReaderT m env) src
  in (fundec { funDefBody = body' }, src')
  where env = Env mempty doNotTouchLoop
        doNotTouchLoop ctx val body = return (mempty, ctx, val, body)

data Env = Env { envScope :: Scope ExplicitMemory
               , envOptimiseLoop :: OptimiseLoop
               }

newtype DoubleBufferM a =
  DoubleBufferM { runDoubleBufferM :: ReaderT Env (State VNameSource) a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadFreshNames)

instance HasScope ExplicitMemory DoubleBufferM where
  askScope = asks envScope

instance LocalScope ExplicitMemory DoubleBufferM where
  localScope scope = local $ \env -> env { envScope = envScope env <> scope }

optimiseBody :: Body ExplicitMemory -> DoubleBufferM (Body ExplicitMemory)
optimiseBody body = do
  bnds' <- optimiseStms $ stmsToList $ bodyStms body
  return $ body { bodyStms = stmsFromList bnds' }

optimiseStms :: [Stm ExplicitMemory] -> DoubleBufferM [Stm ExplicitMemory]
optimiseStms [] = return []
optimiseStms (e:es) = do
  e_es <- optimiseStm e
  es' <- localScope (castScope $ scopeOf e_es) $ optimiseStms es
  return $ e_es ++ es'

optimiseStm :: Stm ExplicitMemory -> DoubleBufferM [Stm ExplicitMemory]
optimiseStm (Let pat aux (DoLoop ctx val form body)) = do
  body' <- localScope (scopeOf form <> scopeOfFParams (map fst $ ctx++val)) $
           optimiseBody body
  opt_loop <- asks envOptimiseLoop
  (bnds, ctx', val', body'') <- opt_loop ctx val body'
  return $ bnds ++ [Let pat aux $ DoLoop ctx' val' form body'']
optimiseStm (Let pat aux e) =
  pure . Let pat aux <$> mapExpM optimise e
  where optimise = identityMapper { mapOnBody = \_ x ->
                                      optimiseBody x :: DoubleBufferM (Body ExplicitMemory)
                                  , mapOnOp = optimiseOp
                                  }

optimiseOp :: Op ExplicitMemory
           -> DoubleBufferM (Op ExplicitMemory)
optimiseOp (Inner (Husk hspace red_op nes ts body)) = do
  body' <- localScope (scopeOfHuskSpace hspace) $ optimiseBody body
  return $ Inner $ Husk hspace red_op nes ts body'
optimiseOp (Inner (SegOp op)) =
  local inSegOp $ Inner . SegOp <$> mapSegOpM mapper op
  where mapper = identitySegOpMapper
                 { mapOnSegOpLambda = optimiseLambda
                 , mapOnSegOpBody = optimiseKernelBody
                 }
        inSegOp env = env { envOptimiseLoop = optimiseLoop }
optimiseOp op = return op

optimiseKernelBody :: KernelBody ExplicitMemory
                   -> DoubleBufferM (KernelBody ExplicitMemory)
optimiseKernelBody kbody = do
  stms' <- optimiseStms $ stmsToList $ kernelBodyStms kbody
  return $ kbody { kernelBodyStms = stmsFromList stms' }

optimiseLambda :: Lambda ExplicitMemory -> DoubleBufferM (Lambda ExplicitMemory)
optimiseLambda lam = do
  body <- localScope (castScope $ scopeOf lam) $ optimiseBody $ lambdaBody lam
  return lam { lambdaBody = body }

type OptimiseLoop =
  [(FParam ExplicitMemory, SubExp)] -> [(FParam ExplicitMemory, SubExp)] -> Body ExplicitMemory
  -> DoubleBufferM ([Stm ExplicitMemory],
                    [(FParam ExplicitMemory, SubExp)],
                    [(FParam ExplicitMemory, SubExp)],
                    Body ExplicitMemory)

optimiseLoop :: OptimiseLoop
optimiseLoop ctx val body = do
  -- We start out by figuring out which of the merge variables should
  -- be double-buffered.
  buffered <- doubleBufferMergeParams
              (zip (map fst ctx) (bodyResult body)) (map fst merge)
              (boundInBody body)
  -- Then create the allocations of the buffers and copies of the
  -- initial values.
  (merge', allocs) <- allocStms merge buffered
  -- Modify the loop body to copy buffered result arrays.
  let body' = doubleBufferResult (map fst merge) buffered body
      (ctx', val') = splitAt (length ctx) merge'
  -- Modify the initial merge p
  return (allocs, ctx', val', body')
  where merge = ctx ++ val

-- | The booleans indicate whether we should also play with the
-- initial merge values.
data DoubleBuffer = BufferAlloc VName (PrimExp VName) Space Bool
                  | BufferCopy VName IxFun VName Bool
                    -- ^ First name is the memory block to copy to,
                    -- second is the name of the array copy.
                  | NoBuffer
                    deriving (Show)

doubleBufferMergeParams :: MonadFreshNames m =>
                           [(FParam ExplicitMemory, SubExp)]
                        -> [FParam ExplicitMemory] -> Names
                        -> m [DoubleBuffer]
doubleBufferMergeParams ctx_and_res val_params bound_in_loop =
  evalStateT (mapM buffer val_params) M.empty
  where loopVariant v = v `S.member` bound_in_loop ||
                        v `elem` map (paramName . fst) ctx_and_res

        loopInvariantSize (Constant v) =
          Just (Constant v, True)
        loopInvariantSize (Var v) =
          case find ((==v) . paramName . fst) ctx_and_res of
            Just (_, Constant val) ->
              Just (Constant val, False)
            Just (_, Var v') | not $ loopVariant v' ->
              Just (Var v', False)
            Just _ ->
              Nothing
            Nothing ->
              Just (Var v, True)

        sizeForMem mem = maybeHead $ mapMaybe (arrayInMem . paramAttr) val_params
          where arrayInMem (MemArray pt shape _ (ArrayIn arraymem ixfun))
                  | IxFun.isDirect ixfun,
                    Just (dims, b) <-
                      mapAndUnzipM loopInvariantSize $ shapeDims shape,
                    mem == arraymem =
                      Just (arraySizeInBytesExp $
                             Array pt (Shape dims) NoUniqueness,
                            or b)
                arrayInMem _ = Nothing

        buffer fparam = case paramType fparam of
          Mem space
            | Just (size, b) <- sizeForMem $ paramName fparam -> do
                -- Let us double buffer this!
                bufname <- lift $ newVName "double_buffer_mem"
                modify $ M.insert (paramName fparam) (bufname, b)
                return $ BufferAlloc bufname size space b
          Array {}
            | MemArray _ _ _ (ArrayIn mem ixfun) <- paramAttr fparam -> do
                buffered <- gets $ M.lookup mem
                case buffered of
                  Just (bufname, b) -> do
                    copyname <- lift $ newVName "double_buffer_array"
                    return $ BufferCopy bufname ixfun copyname b
                  Nothing ->
                    return NoBuffer
          _ -> return NoBuffer

allocStms :: [(FParam ExplicitMemory, SubExp)] -> [DoubleBuffer]
          -> DoubleBufferM ([(FParam ExplicitMemory, SubExp)], [Stm ExplicitMemory])
allocStms merge = runWriterT . zipWithM allocation merge
  where allocation m@(Param pname _, _) (BufferAlloc name size space b) = do
          stms <- lift $ runBinder_ $ do
            size' <- letSubExp "double_buffer_size" =<< toExp size
            letBindNames_ [name] $ Op $ Alloc size' space
          tell $ stmsToList stms
          if b then return (Param pname $ MemMem space, Var name)
               else return m
        allocation (f, Var v) (BufferCopy mem _ _ b) | b = do
          v_copy <- lift $ newVName $ baseString v ++ "_double_buffer_copy"
          (_v_mem, v_ixfun) <- lift $ lookupArraySummary v
          let bt = elemType $ paramType f
              shape = arrayShape $ paramType f
              bound = MemArray bt shape NoUniqueness $ ArrayIn mem v_ixfun
          tell [Let (Pattern [] [PatElem v_copy bound]) (defAux ()) $
                BasicOp $ Copy v]
          return (f, Var v_copy)
        allocation (f, se) _ =
          return (f, se)

doubleBufferResult :: [FParam ExplicitMemory] -> [DoubleBuffer]
                   -> Body ExplicitMemory -> Body ExplicitMemory
doubleBufferResult valparams buffered (Body () bnds res) =
  let (ctx_res, val_res) = splitAt (length res - length valparams) res
      (copybnds,val_res') =
        unzip $ zipWith3 buffer valparams buffered val_res
  in Body () (bnds<>stmsFromList (catMaybes copybnds)) $ ctx_res ++ val_res'
  where buffer _ (BufferAlloc bufname _ _ _) _ =
          (Nothing, Var bufname)

        buffer fparam (BufferCopy bufname ixfun copyname _) (Var v) =
          -- To construct the copy we will need to figure out its type
          -- based on the type of the function parameter.
          let t = resultType $ paramType fparam
              summary = MemArray (elemType t) (arrayShape t) NoUniqueness $ ArrayIn bufname ixfun
              copybnd = Let (Pattern [] [PatElem copyname summary]) (defAux ()) $
                        BasicOp $ Copy v
          in (Just copybnd, Var copyname)

        buffer _ _ se =
          (Nothing, se)

        parammap = M.fromList $ zip (map paramName valparams) res

        resultType t = t `setArrayDims` map substitute (arrayDims t)

        substitute (Var v)
          | Just replacement <- M.lookup v parammap = replacement
        substitute se =
          se
