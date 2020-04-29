module Futhark.Pass.ExtractMulticore (extractMulticore) where

import Control.Monad.Reader
import Control.Monad.State

import Futhark.Analysis.Rephrase
import Futhark.Tools
import Futhark.MonadFreshNames
import Futhark.Pass
import Futhark.Representation.AST
import Futhark.Representation.MC
import Futhark.Representation.SOACS hiding (Body, Exp, Lambda, LParam, Stm)
{-
type ExtractM = ReaderT (Scope MC) (State VNameSource)

transformExp :: Exp SOACS -> ExtractM (Exp MC)
transformExp (BasicOp op) = return $ BasicOp op

transformStm :: Stm SOACS -> ExtractM (Stm MC)
transformStm (Let pat aux e) = Let pat aux <$> transformExp e

transformStms :: Stms SOACS -> ExtractM (Stms MC)
transformStms stms =
  case stmsHead stms of
    Nothing -> return mempty
    Just (stm, stms') -> do
      stm' <- transformStm stm
      inScopeOf stm' $ (oneStm stm'<>) <$> transformStms stms'

transformBody :: Body SOACS -> ExtractM (Body MC)
transformBody (Body () stms res) =
  Body () <$> transformStms stms <*> pure res

transformFunDef :: FunDef SOACS -> ExtractM (FunDef MC)
transformFunDef (FunDef entry name rettype params body) = do
  body' <- localScope (scopeOfFParams params) $
           transformBody body
  return $ FunDef entry name rettype params body'

transformProg :: Prog SOACS -> PassM (Prog MC)
transformProg (Prog consts funs) =
  modifyNameSource $ runState (runReaderT m mempty)
  where m = do
          consts' <- transformStms consts
          funs' <- inScopeOf consts' $ mapM transformFunDef funs
          return $ Prog consts' funs'
-}

indexArray :: VName -> LParam SOACS -> VName -> Stm MC
indexArray i (Param p t) arr =
  Let (Pattern [] [PatElem p t]) (defAux ()) $
  BasicOp $ Index arr $ DimFix (Var i) : map sliceDim (arrayDims t)

mapLambdaToKernelBody :: MonadFreshNames m =>
                         VName -> Lambda SOACS -> [VName] -> m (KernelBody MC)
mapLambdaToKernelBody i lam arrs = do
  Body () stms res <- rephraseBody rephraser $ lambdaBody lam
  let indexings = zipWith (indexArray i) (lambdaParams lam) arrs
  return $ KernelBody () (stmsFromList indexings <> stms) $
    map (Returns ResultMaySimplify) res

reduceToSegRedOp :: MonadFreshNames m => Reduce SOACS -> m (SegRedOp MC)
reduceToSegRedOp (Reduce comm lam nes) =
  SegRedOp comm <$> rephraseLambda rephraser lam <*> pure nes <*> pure mempty

soacToSegOp :: MonadFreshNames m => SOAC SOACS -> m (SegOp () MC)
soacToSegOp screma@(Screma w form arrs) = do
  flat <- newVName "flat_tid"
  gtid <- newVName "gtid"
  let space = SegSpace flat [(gtid, w)]

  case () of
    _ | Just map_lam <- isMapSOAC form -> do
          kbody <- mapLambdaToKernelBody gtid map_lam arrs
          pure $ SegMap () space (lambdaReturnType map_lam) kbody

      | Just (reds, map_lam) <- isRedomapSOAC form -> do
          kbody <- mapLambdaToKernelBody gtid map_lam arrs
          reds' <- mapM reduceToSegRedOp reds
          pure $ SegRed () space reds' (lambdaReturnType map_lam) kbody

      | Just (lam, nes, map_lam) <- isScanomapSOAC form -> do
          kbody <- mapLambdaToKernelBody gtid map_lam arrs
          lam' <- rephraseLambda rephraser lam
          pure $ SegScan () space lam' nes (lambdaReturnType map_lam) kbody

      | otherwise -> error $ "Too complex Screma: " ++ pretty screma

rephraser :: MonadFreshNames m => Rephraser m SOACS MC
rephraser =
  Rephraser
  { rephraseExpLore = pure
  , rephraseLetBoundLore = pure
  , rephraseFParamLore = pure
  , rephraseLParamLore = pure
  , rephraseBodyLore = pure
  , rephraseRetType = pure
  , rephraseBranchType = pure
  , rephraseOp = soacToSegOp
  }

extractMulticore :: Pass SOACS MC
extractMulticore =
  Pass { passName = "extract multicore parallelism"
       , passDescription = "Extract multicore parallelism"
       , passFunction = rephraseProg rephraser
       }
