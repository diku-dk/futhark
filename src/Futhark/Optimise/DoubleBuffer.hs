{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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

import           Futhark.MonadFreshNames
import           Futhark.Representation.AST
import           Futhark.Representation.ExplicitMemory
                 hiding (Prog, Body, Stm, Pattern, PatElem,
                         BasicOp, Exp, Lambda, FunDef, FParam, LParam, RetType)
import           Futhark.Pass

doubleBuffer :: Pass ExplicitMemory ExplicitMemory
doubleBuffer =
  Pass { passName = "Double buffer"
       , passDescription = "Perform double buffering for merge parameters of sequential loops."
       , passFunction = intraproceduralTransformation optimiseFunDef
       }

-- This pass is written in a slightly weird way because we want to
-- apply essentially the same transformation both outside and inside
-- kernel bodies, which are different (but similar) representations.
-- Thus, the environment is parametrised by the lore and contains the
-- function used to transform 'Op's for the lore.

optimiseFunDef :: FunDef ExplicitMemory -> PassM (FunDef ExplicitMemory)
optimiseFunDef fundec = modifyNameSource $ \src ->
  let m = runDoubleBufferM $ inScopeOf fundec $ optimiseBody $ funDefBody fundec
      (body', src') = runState (runReaderT m env) src
  in (fundec { funDefBody = body' }, src')
  where env = Env mempty optimiseKernelOp optimiseLoopOutsideKernel

        optimiseKernelOp (Inner k) = do
          scope <- castScope <$> askScope
          modifyNameSource $
            runState (runReaderT (runDoubleBufferM $ Inner <$> optimiseKernel k) $
                      Env scope optimiseInKernelOp optimiseLoopInKernel)
          where optimiseKernel =
                  mapKernelM identityKernelMapper
                  { mapOnKernelBody = optimiseBody
                  , mapOnKernelKernelBody = optimiseKernelBody
                  , mapOnKernelLambda = optimiseLambda
                  }
        optimiseKernelOp op = return op

        optimiseInKernelOp (Inner (GroupStream w maxchunk lam accs arrs)) = do
          lam' <- optimiseGroupStreamLambda lam
          return $ Inner $ GroupStream w maxchunk lam' accs arrs
        optimiseInKernelOp op = return op

data Env lore = Env { envScope :: Scope lore
                    , envOptimiseOp :: Op lore -> DoubleBufferM lore (Op lore)
                    , envOptimiseLoop :: OptimiseLoop lore
                    }

newtype DoubleBufferM lore a =
  DoubleBufferM { runDoubleBufferM :: ReaderT (Env lore) (State VNameSource) a }
  deriving (Functor, Applicative, Monad, MonadReader (Env lore), MonadFreshNames)

instance Annotations lore => HasScope lore (DoubleBufferM lore) where
  askScope = asks envScope

instance Annotations lore => LocalScope lore (DoubleBufferM lore) where
  localScope scope = local $ \env -> env { envScope = envScope env <> scope }

-- | Bunch up all the constraints for less typing.
type LoreConstraints lore inner =
  (ExpAttr lore ~ (), BodyAttr lore ~ (),
   ExplicitMemorish lore, Op lore ~ MemOp inner)

optimiseBody :: LoreConstraints lore inner =>
                Body lore -> DoubleBufferM lore (Body lore)
optimiseBody body = do
  bnds' <- optimiseStms $ stmsToList $ bodyStms body
  return $ body { bodyStms = stmsFromList bnds' }

optimiseStms :: LoreConstraints lore inner =>
                [Stm lore] -> DoubleBufferM lore [Stm lore]
optimiseStms [] = return []
optimiseStms (e:es) = do
  e_es <- optimiseStm e
  es' <- localScope (castScope $ scopeOf e_es) $ optimiseStms es
  return $ e_es ++ es'

optimiseStm :: LoreConstraints lore inner =>
               Stm lore -> DoubleBufferM lore [Stm lore]
optimiseStm (Let pat aux (DoLoop ctx val form body)) = do
  body' <- localScope (scopeOf form <> scopeOfFParams (map fst $ ctx++val)) $
           optimiseBody body
  opt_loop <- asks envOptimiseLoop
  (bnds, ctx', val', body'') <- opt_loop ctx val body'
  return $ bnds ++ [Let pat aux $ DoLoop ctx' val' form body'']
optimiseStm (Let pat aux e) =
  pure . Let pat aux <$> mapExpM optimise e
  where optimise = identityMapper { mapOnBody = const optimiseBody
                                  , mapOnOp = optimiseOp
                                  }

optimiseOp :: Op lore -> DoubleBufferM lore (Op lore)
optimiseOp op = do f <- asks envOptimiseOp
                   f op

optimiseKernelBody :: KernelBody InKernel
                   -> DoubleBufferM InKernel (KernelBody InKernel)
optimiseKernelBody kbody = do
  stms' <- optimiseStms $ stmsToList $ kernelBodyStms kbody
  return $ kbody { kernelBodyStms = stmsFromList stms' }

optimiseLambda :: Lambda InKernel -> DoubleBufferM InKernel (Lambda InKernel)
optimiseLambda lam = do
  body <- localScope (castScope $ scopeOf lam) $ optimiseBody $ lambdaBody lam
  return lam { lambdaBody = body }

optimiseGroupStreamLambda :: GroupStreamLambda InKernel
                          -> DoubleBufferM InKernel (GroupStreamLambda InKernel)
optimiseGroupStreamLambda lam = do
  body <- localScope (scopeOf lam) $
          optimiseBody $ groupStreamLambdaBody lam
  return lam { groupStreamLambdaBody = body }

type OptimiseLoop lore =
  [(FParam lore, SubExp)] -> [(FParam lore, SubExp)] -> Body lore
  -> DoubleBufferM lore ([Stm lore],
                         [(FParam lore, SubExp)],
                         [(FParam lore, SubExp)],
                         Body lore)

optimiseLoop :: LoreConstraints lore inner => Bool -> OptimiseLoop lore
optimiseLoop copy_init ctx val body = do
  -- We start out by figuring out which of the merge variables should
  -- be double-buffered.
  buffered <- doubleBufferMergeParams
              copy_init
              (zip (map fst ctx) (bodyResult body))
              (map fst merge)
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

optimiseLoopOutsideKernel :: OptimiseLoop ExplicitMemory
optimiseLoopOutsideKernel = optimiseLoop False

optimiseLoopInKernel :: OptimiseLoop InKernel
optimiseLoopInKernel = optimiseLoop True

-- | The booleans indicate whether we should also play with the
-- initial merge values.
data DoubleBuffer lore = BufferAlloc VName SubExp Space Bool
                       | BufferCopy VName IxFun VName Bool
                       -- ^ First name is the memory block to copy to,
                       -- second is the name of the array copy.
                       | NoBuffer
                    deriving (Show)

doubleBufferMergeParams :: (ExplicitMemorish lore, MonadFreshNames m) =>
                           Bool -> [(FParam lore,SubExp)]
                        -> [FParam lore] -> Names
                        -> m [DoubleBuffer lore]
doubleBufferMergeParams copy_init ctx_and_res val_params bound_in_loop =
  evalStateT (mapM buffer val_params) M.empty
  where loopVariant v = v `S.member` bound_in_loop ||
                        v `elem` map (paramName . fst) ctx_and_res

        loopInvariantSize (Constant v) =
          Just (Constant v, copy_init)
        loopInvariantSize (Var v) =
          case find ((==v) . paramName . fst) ctx_and_res of
            Just (_, Constant val) ->
              Just (Constant val, False)
            Just (_, Var v') | not $ loopVariant v' ->
              Just (Var v', False)
            Just _ ->
              Nothing
            Nothing ->
              Just (Var v, copy_init)

        buffer fparam = case paramType fparam of
          Mem size space
            | Just (size', b) <- loopInvariantSize size -> do
                -- Let us double buffer this!
                bufname <- lift $ newVName "double_buffer_mem"
                modify $ M.insert (paramName fparam) (bufname, b)
                return $ BufferAlloc bufname size' space b
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

allocStms :: LoreConstraints lore inner =>
             [(FParam lore,SubExp)] -> [DoubleBuffer lore]
          -> DoubleBufferM lore ([(FParam lore, SubExp)], [Stm lore])
allocStms merge = runWriterT . zipWithM allocation merge
  where allocation m@(Param pname _, _) (BufferAlloc name size space b) = do
          tell [Let (Pattern [] [PatElem name $ MemMem size space]) (defAux ()) $
                Op $ Alloc size space]
          if b then return (Param pname $ MemMem size space, Var name)
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

doubleBufferResult :: (ExplicitMemorish lore,
                       ExpAttr lore ~ (), BodyAttr lore ~ ()) =>
                      [FParam lore] -> [DoubleBuffer lore]
                   -> Body lore -> Body lore
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
