{-# LANGUAGE TypeFamilies #-}

-- The idea is to perform distribution on one level at a time, and
-- produce "irregular Maps" that can accept and produce irregular
-- arrays.  These irregular maps will then be transformed into flat
-- parallelism based on their contents.  This is a sensitive detail,
-- but if irregular maps contain only a single Stm, then it is fairly
-- straightforward, as we simply implement flattening rules for every
-- single kind of expression.  Of course that is also somewhat
-- inefficient, so we want to support multiple Stms for things like
-- scalar code.
module Futhark.Pass.Flatten (flattenSOACs) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (bimap, first, second)
import Data.Foldable
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Debug.Trace
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Pass
import Futhark.Pass.ExtractKernels.BlockedKernel (mkSegSpace, segScan)
import Futhark.Pass.ExtractKernels.ToGPU (scopeForGPU, soacsLambdaToGPU, soacsStmToGPU)
import Futhark.Pass.Flatten.Builtins
import Futhark.Pass.Flatten.Distribute
import Futhark.Tools

data FlattenEnv = FlattenEnv

newtype FlattenM a = FlattenM (StateT VNameSource (Reader FlattenEnv) a)
  deriving
    ( MonadState VNameSource,
      MonadFreshNames,
      MonadReader FlattenEnv,
      Monad,
      Functor,
      Applicative
    )

data IrregularRep = IrregularRep
  { irregularSegments :: VName,
    irregularFlags :: VName,
    irregularOffsets :: VName,
    iregularElems :: VName
  }

data ResRep
  = -- | This variable is represented
    -- completely straightforwardly- if it is
    -- an array, it is a regular array.
    Regular VName
  | -- | The representation of an
    -- irregular array.
    Irregular IrregularRep

data DistEnv = DistEnv {distResMap :: M.Map ResTag ResRep}

insertRep :: ResTag -> ResRep -> DistEnv -> DistEnv
insertRep rt rep env = env {distResMap = M.insert rt rep $ distResMap env}

insertReps :: [(ResTag, ResRep)] -> DistEnv -> DistEnv
insertReps = flip $ foldl (flip $ uncurry insertRep)

instance Monoid DistEnv where
  mempty = DistEnv mempty

instance Semigroup DistEnv where
  DistEnv x <> DistEnv y = DistEnv (x <> y)

flagsAndElems :: DistEnv -> [DistInput] -> (Maybe (VName, VName), [VName])
flagsAndElems env [] = (Nothing, [])
flagsAndElems env (DistInputFree v _ : vs) =
  second (v :) $ flagsAndElems env vs
flagsAndElems env (DistInput rt _ : vs) =
  case M.lookup rt $ distResMap env of
    Just (Regular v') ->
      second (v' :) $ flagsAndElems env vs
    Just (Irregular (IrregularRep _ flags offsets elems)) ->
      bimap (mplus $ Just (flags, offsets)) (elems :) $ flagsAndElems env vs
    _ ->
      error "flagsAndElems: nope"

transformDistStm :: DistEnv -> DistStm -> Builder GPU DistEnv
transformDistStm env (DistStm inps res stm) =
  case stm of
    Let _ _ (BasicOp (Iota (Var n) x s Int64))
      | Just (DistInputFree ns _) <- lookup n inps -> do
          let ~[DistResult rt _] = res
          (flags, offsets, elems) <- doSegIota ns
          let rep = Irregular $ IrregularRep ns flags offsets elems
          pure $ insertRep rt rep env
    Let _ _ (Op (Screma w arrs form))
      | Just reds <- isReduceSOAC form,
        Just arrs' <- mapM (`lookup` inps) arrs,
        (Just (flags, offsets), elems) <- flagsAndElems env arrs' -> do
          elems' <- genSegRed flags offsets elems $ singleReduce reds
          pure $ insertReps (zip (map distResTag res) (map Regular elems')) env
    _ -> error $ "Unhandled:\n" ++ prettyString stm

transformDistributed :: Distributed -> Builder GPU ()
transformDistributed (Distributed dstms resmap) = do
  env <- foldM transformDistStm mempty dstms
  forM_ (M.toList resmap) $ \(rt, v) -> do
    case M.lookup rt $ distResMap env of
      Just (Regular v') -> letBindNames [v] $ BasicOp $ SubExp $ Var v'
      Just Irregular {} -> error $ "Result is irregular: " ++ prettyString v
      Nothing -> error $ "Missing result binding: " ++ prettyString v

transformStm :: Scope SOACS -> Stm SOACS -> PassM (Stms GPU)
transformStm scope (Let pat _ (Op (Screma w arrs form)))
  | Just lam <- isMapSOAC form = do
      let distributed = distributeMap scope pat w arrs lam
          m = transformDistributed distributed
      traceM $ prettyString distributed
      runReaderT (runBuilder_ m) scope
transformStm scope stm = pure $ oneStm $ soacsStmToGPU stm

transformStms :: Scope SOACS -> Stms SOACS -> PassM (Stms GPU)
transformStms scope stms =
  fold <$> traverse (transformStm (scope <> scopeOf stms)) stms

transformFunDef :: Scope SOACS -> FunDef SOACS -> PassM (FunDef GPU)
transformFunDef consts_scope fd = do
  let FunDef
        { funDefBody = Body () stms res,
          funDefParams = fparams,
          funDefRetType = rettype
        } = fd
  stms' <- transformStms (consts_scope <> scopeOfFParams fparams) stms
  pure $
    fd
      { funDefBody = Body () stms' res,
        funDefRetType = rettype,
        funDefParams = fparams
      }

transformProg :: Prog SOACS -> PassM (Prog GPU)
transformProg prog = do
  consts' <- transformStms mempty $ progConsts prog
  funs' <- mapM (transformFunDef $ scopeOf (progConsts prog)) $ progFuns prog
  pure $ prog {progConsts = consts', progFuns = flatteningBuiltins <> funs'}

-- | Transform a SOACS program to a GPU program, using flattening.
flattenSOACs :: Pass SOACS GPU
flattenSOACs =
  Pass
    { passName = "flatten",
      passDescription = "Perform full flattening",
      passFunction = transformProg
    }
{-# NOINLINE flattenSOACs #-}
