{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Facilities for changing the lore of some fragment, with no context.
module Futhark.Analysis.Rephrase
       ( rephraseProg
       , rephraseFunDef
       , rephraseExp
       , rephraseBody
       , rephraseStm
       , rephraseLambda
       , rephrasePattern
       , rephrasePatElem
       , Rephraser (..)

       , castStm
       )
where

import Futhark.Representation.AST

data Rephraser m from to
  = Rephraser { rephraseExpLore :: ExpAttr from -> m (ExpAttr to)
              , rephraseLetBoundLore :: LetAttr from -> m (LetAttr to)
              , rephraseFParamLore :: FParamAttr from -> m (FParamAttr to)
              , rephraseLParamLore :: LParamAttr from -> m (LParamAttr to)
              , rephraseBodyLore :: BodyAttr from -> m (BodyAttr to)
              , rephraseRetType :: RetType from -> m (RetType to)
              , rephraseBranchType :: BranchType from -> m (BranchType to)
              , rephraseOp :: Op from -> m (Op to)
              }

rephraseProg :: Monad m => Rephraser m from to -> Prog from -> m (Prog to)
rephraseProg rephraser = fmap Prog . mapM (rephraseFunDef rephraser) . progFuns

rephraseFunDef :: Monad m => Rephraser m from to -> FunDef from -> m (FunDef to)
rephraseFunDef rephraser fundec = do
  body' <- rephraseBody rephraser $ funDefBody fundec
  params' <- mapM (rephraseParam $ rephraseFParamLore rephraser) $ funDefParams fundec
  rettype' <- mapM (rephraseRetType rephraser) $ funDefRetType fundec
  return fundec { funDefBody = body', funDefParams = params', funDefRetType = rettype' }

rephraseExp :: Monad m => Rephraser m from to -> Exp from -> m (Exp to)
rephraseExp = mapExpM . mapper

rephraseStm :: Monad m => Rephraser m from to -> Stm from -> m (Stm to)
rephraseStm rephraser (Let pat (StmAux cs attr) e) =
  Let <$>
  rephrasePattern (rephraseLetBoundLore rephraser) pat <*>
  (StmAux cs <$> rephraseExpLore rephraser attr) <*>
  rephraseExp rephraser e

rephrasePattern :: Monad m =>
                   (from -> m to)
                -> PatternT from
                -> m (PatternT to)
rephrasePattern f (Pattern context values) =
  Pattern <$> rephrase context <*> rephrase values
  where rephrase = mapM $ rephrasePatElem f

rephrasePatElem :: Monad m => (from -> m to) -> PatElemT from -> m (PatElemT to)
rephrasePatElem rephraser (PatElem ident from) =
  PatElem ident <$> rephraser from

rephraseParam :: Monad m => (from -> m to) -> Param from -> m (Param to)
rephraseParam rephraser (Param name from) =
  Param name <$> rephraser from

rephraseBody :: Monad m => Rephraser m from to -> Body from -> m (Body to)
rephraseBody rephraser (Body lore bnds res) =
  Body <$>
  rephraseBodyLore rephraser lore <*>
  (stmsFromList <$> mapM (rephraseStm rephraser) (stmsToList bnds)) <*>
  pure res

rephraseLambda :: Monad m => Rephraser m from to -> Lambda from -> m (Lambda to)
rephraseLambda rephraser lam = do
  body' <- rephraseBody rephraser $ lambdaBody lam
  params' <- mapM (rephraseParam $ rephraseLParamLore rephraser) $ lambdaParams lam
  return lam { lambdaBody = body', lambdaParams = params' }

mapper :: Monad m => Rephraser m from to -> Mapper from to m
mapper rephraser = identityMapper {
    mapOnBody = const $ rephraseBody rephraser
  , mapOnRetType = rephraseRetType rephraser
  , mapOnBranchType = rephraseBranchType rephraser
  , mapOnFParam = rephraseParam (rephraseFParamLore rephraser)
  , mapOnLParam = rephraseParam (rephraseLParamLore rephraser)
  , mapOnOp = rephraseOp rephraser
  }

-- | Convert a binding from one lore to another, if possible.
castStm :: (SameScope from to,
            ExpAttr from ~ ExpAttr to,
            BodyAttr from ~ BodyAttr to,
            RetType from ~ RetType to,
            BranchType from ~ BranchType to) =>
           Stm from -> Maybe (Stm to)
castStm = rephraseStm caster
  where caster = Rephraser { rephraseExpLore = Just
                           , rephraseBodyLore = Just
                           , rephraseLetBoundLore = Just
                           , rephraseFParamLore = Just
                           , rephraseLParamLore = Just
                           , rephraseOp = const Nothing
                           , rephraseRetType = Just
                           , rephraseBranchType = Just
                           }
