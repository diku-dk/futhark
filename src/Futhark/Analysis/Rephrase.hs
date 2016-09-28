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
       , rephraseExtLambda
       , rephrasePattern
       , rephrasePatElem
       , Rephraser (..)

       , castStm
       )
where

import Control.Applicative

import Prelude

import Futhark.Representation.AST

data Rephraser m from to
  = Rephraser { rephraseExpLore :: ExpAttr from -> m (ExpAttr to)
              , rephraseLetBoundLore :: LetAttr from -> m (LetAttr to)
              , rephraseFParamLore :: FParamAttr from -> m (FParamAttr to)
              , rephraseLParamLore :: LParamAttr from -> m (LParamAttr to)
              , rephraseBodyLore :: BodyAttr from -> m (BodyAttr to)
              , rephraseRetType :: RetType from -> m (RetType to)
              , rephraseOp :: Op from -> m (Op to)
              }

rephraseProg :: (Applicative m, Monad m) => Rephraser m from to -> Prog from -> m (Prog to)
rephraseProg rephraser = fmap Prog . mapM (rephraseFunDef rephraser) . progFunctions

rephraseFunDef :: (Applicative m, Monad m) => Rephraser m from to -> FunDef from -> m (FunDef to)
rephraseFunDef rephraser fundec = do
  body' <- rephraseBody rephraser $ funDefBody fundec
  params' <- mapM (rephraseParam $ rephraseFParamLore rephraser) $ funDefParams fundec
  rettype' <- rephraseRetType rephraser $ funDefRetType fundec
  return fundec { funDefBody = body', funDefParams = params', funDefRetType = rettype' }

rephraseExp :: (Applicative m, Monad m) => Rephraser m from to -> Exp from -> m (Exp to)
rephraseExp = mapExpM . mapper

rephraseStm :: (Applicative m, Monad m) => Rephraser m from to -> Stm from -> m (Stm to)
rephraseStm rephraser (Let pat lore e) =
  Let <$>
  rephrasePattern (rephraseLetBoundLore rephraser) pat <*>
  rephraseExpLore rephraser lore <*>
  rephraseExp rephraser e

rephrasePattern :: (Applicative m, Monad m) =>
                   (from -> m to)
                -> PatternT from
                -> m (PatternT to)
rephrasePattern f (Pattern context values) =
  Pattern <$> rephrase context <*> rephrase values
  where rephrase = mapM $ rephrasePatElem f

rephrasePatElem :: (Applicative m, Monad m) => (from -> m to) -> PatElemT from -> m (PatElemT to)
rephrasePatElem rephraser (PatElem ident BindVar from) =
  PatElem ident BindVar <$> rephraser from
rephrasePatElem rephraser (PatElem ident (BindInPlace cs src is) from) =
  PatElem ident (BindInPlace cs src is) <$> rephraser from

rephraseParam :: (Applicative m, Monad m) => (from -> m to) -> ParamT from -> m (ParamT to)
rephraseParam rephraser (Param name from) =
  Param name <$> rephraser from

rephraseBody :: (Applicative m, Monad m) => Rephraser m from to -> Body from -> m (Body to)
rephraseBody rephraser (Body lore bnds res) =
  Body <$>
  rephraseBodyLore rephraser lore <*>
  mapM (rephraseStm rephraser) bnds <*>
  pure res

rephraseLambda :: (Applicative m, Monad m) => Rephraser m from to -> Lambda from -> m (Lambda to)
rephraseLambda rephraser lam = do
  body' <- rephraseBody rephraser $ lambdaBody lam
  params' <- mapM (rephraseParam $ rephraseLParamLore rephraser) $ lambdaParams lam
  return lam { lambdaBody = body', lambdaParams = params' }

rephraseExtLambda :: (Applicative m, Monad m) => Rephraser m from to -> ExtLambda from -> m (ExtLambda to)
rephraseExtLambda rephraser lam = do
  body' <- rephraseBody rephraser $ extLambdaBody lam
  params' <- mapM (rephraseParam $ rephraseLParamLore rephraser) $ extLambdaParams lam
  return lam { extLambdaBody = body', extLambdaParams = params' }

mapper :: (Applicative m, Monad m) => Rephraser m from to -> Mapper from to m
mapper rephraser = identityMapper {
    mapOnBody = rephraseBody rephraser
  , mapOnRetType = rephraseRetType rephraser
  , mapOnFParam = rephraseParam (rephraseFParamLore rephraser)
  , mapOnOp = rephraseOp rephraser
  }

-- | Convert a binding from one lore to another, if possible.
castStm :: (SameScope from to,
            ExpAttr from ~ ExpAttr to,
            BodyAttr from ~ BodyAttr to,
            RetType from ~ RetType to) =>
           Stm from -> Maybe (Stm to)
castStm = rephraseStm caster

caster :: (SameScope from to,
           ExpAttr from ~ ExpAttr to,
           BodyAttr from ~ BodyAttr to,
           RetType from ~ RetType to) =>
          Rephraser Maybe from to
caster = Rephraser { rephraseExpLore = Just
                   , rephraseBodyLore = Just
                   , rephraseLetBoundLore = Just
                   , rephraseFParamLore = Just
                   , rephraseLParamLore = Just
                   , rephraseOp = const Nothing
                   , rephraseRetType = Just
                   }
