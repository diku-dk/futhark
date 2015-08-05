{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- | Expand allocations inside of maps when possible.
module Futhark.ExpandAllocations
       ( expandAllocations )
       where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe
import Data.List

import Prelude

import qualified Futhark.Analysis.ScalExp as SE
import Futhark.MonadFreshNames
import Futhark.Representation.ExplicitMemory
import Futhark.Tools
import Futhark.Util
import qualified Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe as IxFun

expandAllocations :: Prog -> Prog
expandAllocations = intraproceduralTransformation transformFunDec

transformFunDec :: MonadFreshNames m => FunDec -> m FunDec
transformFunDec fundec = do
  body' <- modifyNameSource $ runState m
  return fundec { funDecBody = body' }
  where m = transformBody $ funDecBody fundec

type ExpandM = State VNameSource

transformBody :: Body -> ExpandM Body
transformBody (Body () bnds res) = do
  bnds' <- concat <$> mapM transformBinding bnds
  return $ Body () bnds' res

transformBinding :: Binding -> ExpandM [Binding]

transformBinding (Let pat () e) = do
  (bnds, e') <- transformExp =<< mapExpM transform e
  return $ bnds ++ [Let pat () e']
  where transform = identityMapper { mapOnBody = transformBody
                                   , mapOnLambda = transformLambda
                                   , mapOnExtLambda = transformExtLambda
                                   }

transformExp :: Exp -> ExpandM ([Binding], Exp)
transformExp (LoopOp (Map cs w fun args))
  -- Extract allocations from the body.
  | Right (body, thread_allocs) <- extractKernelAllocations fun = do
  -- We expand the allocations by multiplying their size with the
  -- number of kernel threads.
  alloc_bnds <-
    liftM concat $ forM (HM.toList thread_allocs) $ \(mem,per_thread_size) -> do
      total_size <- newVName "total_size"
      let sizepat = Pattern [] [PatElem (Ident total_size $ Basic Int) BindVar Scalar]
          allocpat = Pattern [] [PatElem (Ident mem $ Mem $ Var total_size) BindVar Scalar]
      return [Let sizepat () $ PrimOp $ BinOp Times w per_thread_size Int,
              Let allocpat () $ PrimOp $ Alloc $ Var total_size]

  -- Fix every reference to the memory blocks to be offset by the
  -- thread number.
  let thread_num = lambdaIndex fun
      alloc_offsets =
        OffsetMap { offsetMap =
                    HM.map (SE.STimes (SE.Id thread_num Int) . SE.intSubExpToScalExp)
                    thread_allocs
                  , indexVariable = lambdaIndex fun
                  , kernelWidth = w
                  }
      fun' = fun { lambdaBody =
                       offsetMemorySummariesInBody alloc_offsets body
                 }
  return (alloc_bnds, LoopOp (Map cs w fun' args))
transformExp e =
  return ([], e)

transformLambda :: Lambda -> ExpandM Lambda
transformLambda lam = do
  body' <- transformBody $ lambdaBody lam
  return lam { lambdaBody = body' }

transformExtLambda :: ExtLambda -> ExpandM ExtLambda
transformExtLambda lam = do
  body' <- transformBody $ extLambdaBody lam
  return lam { extLambdaBody = body' }

-- | Returns a map from memory block names to their size in bytes,
-- as well as the lambda body where all the allocations have been removed.
-- Only looks at allocations in the immediate body - if there are any
-- further down, we will fail later.  If the size of one of the
-- allocations is not free in the lambda, we return 'Left' and an
-- error message.
extractKernelAllocations :: Lambda -> Either String (Body, HM.HashMap VName SubExp)
extractKernelAllocations lam = do
  (allocs, bnds) <- mapAccumLM isAlloc HM.empty lambdaBindings
  return ((lambdaBody lam) { bodyBindings = catMaybes bnds }, allocs)
  where boundHere = HS.fromList $
                    map paramName (lambdaParams lam) ++
                    concatMap (patternNames . bindingPattern) lambdaBindings

        lambdaBindings = bodyBindings $ lambdaBody lam

        isAlloc _ (Let (Pattern [] [patElem]) () (PrimOp (Alloc (Var v))))
          | v `HS.member` boundHere =
            throwError $ "Size " ++ pretty v ++
            " for block " ++ pretty patElem ++
            " is not lambda-invariant"

        isAlloc allocs (Let (Pattern [] [patElem]) () (PrimOp (Alloc size))) =
          return (HM.insert (patElemName patElem) size allocs, Nothing)

        isAlloc allocs bnd =
          return (allocs, Just bnd)

data OffsetMap = OffsetMap {
    offsetMap :: HM.HashMap VName SE.ScalExp
    -- ^ A map from memory block names to offsets.
  , indexVariable :: VName
  , kernelWidth :: SubExp
  }

lookupOffset :: VName -> OffsetMap -> Maybe SE.ScalExp
lookupOffset name = HM.lookup name . offsetMap

offsetByIndex :: VName -> SubExp -> OffsetMap -> OffsetMap
offsetByIndex name size (OffsetMap offsets index width) =
  OffsetMap (HM.insert name offset offsets) index width
  where offset = SE.intSubExpToScalExp size /
                 SE.intSubExpToScalExp width * SE.Id index Int

offsetMemorySummariesInBody :: OffsetMap -> Body -> Body
offsetMemorySummariesInBody offsets (Body attr bnds res) =
  Body attr (snd $ mapAccumL offsetMemorySummariesInBinding offsets bnds) res

offsetMemorySummariesInBinding :: OffsetMap -> Binding
                               -> (OffsetMap, Binding)
offsetMemorySummariesInBinding offsets (Let pat attr e) =
  (offsets', Let pat' attr $ offsetMemorySummariesInExp offsets e)
  where (offsets', pat') = offsetMemorySummariesInPattern offsets pat

offsetMemorySummariesInPattern :: OffsetMap -> Pattern -> (OffsetMap, Pattern)
offsetMemorySummariesInPattern offsets (Pattern ctx vals) =
  (offsets', Pattern ctx vals')
  where offsets' = foldl inspectCtx offsets ctx
        vals' = map inspectVal vals
        inspectVal patElem =
          patElem { patElemLore =
                       offsetMemorySummariesInMemSummary offsets' $ patElemLore patElem
                  }
        inspectCtx ctx_offsets patElem
          | Mem size <- patElemType patElem =
              offsetByIndex (patElemName patElem) size ctx_offsets
          | otherwise =
              ctx_offsets

offsetMemorySummariesInFParam :: OffsetMap -> FParam -> FParam
offsetMemorySummariesInFParam offsets fparam =
  fparam { paramLore = offsetMemorySummariesInMemSummary offsets $ paramLore fparam }

offsetMemorySummariesInMemSummary :: OffsetMap -> MemSummary -> MemSummary
offsetMemorySummariesInMemSummary offsets (MemSummary mem ixfun)
  | Just offset <- lookupOffset mem offsets,
    -- XXX FIXME HACK - the problem is making sure multiple runs of
    -- the pass won't mess up anything.
    IxFun.underlyingOffset ixfun == 0 =
      MemSummary mem $ IxFun.offsetUnderlying ixfun offset
offsetMemorySummariesInMemSummary _ summary =
  summary

offsetMemorySummariesInExp :: OffsetMap -> Exp -> Exp
offsetMemorySummariesInExp offsets (LoopOp (DoLoop res merge form body)) =
  LoopOp $ DoLoop res (zip mergeparams' mergeinit) form body'
  where (mergeparams, mergeinit) = unzip merge
        body' = offsetMemorySummariesInBody offsets body
        mergeparams' = map (offsetMemorySummariesInFParam offsets) mergeparams
offsetMemorySummariesInExp offsets e = mapExp recurse e
  where recurse = identityMapper { mapOnBody = return . offsetMemorySummariesInBody offsets
                                 }
