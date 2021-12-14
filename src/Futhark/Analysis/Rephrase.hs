{-# LANGUAGE ConstraintKinds #-}

-- | Facilities for changing the rep of some fragment, with no
-- context.  We call this "rephrasing", for no deep reason.
module Futhark.Analysis.Rephrase
  ( rephraseProg,
    rephraseFunDef,
    rephraseExp,
    rephraseBody,
    rephraseStm,
    rephraseLambda,
    rephrasePat,
    rephrasePatElem,
    Rephraser (..),
  )
where

import Futhark.IR

-- | A collection of functions that together allow us to rephrase some
-- IR fragment, in some monad @m@.  If we let @m@ be the 'Maybe'
-- monad, we can conveniently do rephrasing that might fail.  This is
-- useful if you want to see if some IR in e.g. the @Kernels@ rep
-- actually uses any @Kernels@-specific operations.
data Rephraser m from to = Rephraser
  { rephraseExpDec :: ExpDec from -> m (ExpDec to),
    rephraseLetBoundDec :: LetDec from -> m (LetDec to),
    rephraseFParamDec :: FParamInfo from -> m (FParamInfo to),
    rephraseLParamDec :: LParamInfo from -> m (LParamInfo to),
    rephraseBodyDec :: BodyDec from -> m (BodyDec to),
    rephraseRetType :: RetType from -> m (RetType to),
    rephraseBranchType :: BranchType from -> m (BranchType to),
    rephraseOp :: Op from -> m (Op to)
  }

-- | Rephrase an entire program.
rephraseProg :: Monad m => Rephraser m from to -> Prog from -> m (Prog to)
rephraseProg rephraser (Prog consts funs) =
  Prog
    <$> mapM (rephraseStm rephraser) consts
    <*> mapM (rephraseFunDef rephraser) funs

-- | Rephrase a function definition.
rephraseFunDef :: Monad m => Rephraser m from to -> FunDef from -> m (FunDef to)
rephraseFunDef rephraser fundec = do
  body' <- rephraseBody rephraser $ funDefBody fundec
  params' <- mapM (rephraseParam $ rephraseFParamDec rephraser) $ funDefParams fundec
  rettype' <- mapM (rephraseRetType rephraser) $ funDefRetType fundec
  return fundec {funDefBody = body', funDefParams = params', funDefRetType = rettype'}

-- | Rephrase an expression.
rephraseExp :: Monad m => Rephraser m from to -> Exp from -> m (Exp to)
rephraseExp = mapExpM . mapper

-- | Rephrase a statement.
rephraseStm :: Monad m => Rephraser m from to -> Stm from -> m (Stm to)
rephraseStm rephraser (Let pat (StmAux cs attrs dec) e) =
  Let
    <$> rephrasePat (rephraseLetBoundDec rephraser) pat
    <*> (StmAux cs attrs <$> rephraseExpDec rephraser dec)
    <*> rephraseExp rephraser e

-- | Rephrase a pattern.
rephrasePat ::
  Monad m =>
  (from -> m to) ->
  PatT from ->
  m (PatT to)
rephrasePat = traverse

-- | Rephrase a pattern element.
rephrasePatElem :: Monad m => (from -> m to) -> PatElemT from -> m (PatElemT to)
rephrasePatElem rephraser (PatElem ident from) =
  PatElem ident <$> rephraser from

-- | Rephrase a parameter.
rephraseParam :: Monad m => (from -> m to) -> Param from -> m (Param to)
rephraseParam rephraser (Param attrs name from) =
  Param attrs name <$> rephraser from

-- | Rephrase a body.
rephraseBody :: Monad m => Rephraser m from to -> Body from -> m (Body to)
rephraseBody rephraser (Body rep stms res) =
  Body
    <$> rephraseBodyDec rephraser rep
    <*> (stmsFromList <$> mapM (rephraseStm rephraser) (stmsToList stms))
    <*> pure res

-- | Rephrase a lambda.
rephraseLambda :: Monad m => Rephraser m from to -> Lambda from -> m (Lambda to)
rephraseLambda rephraser lam = do
  body' <- rephraseBody rephraser $ lambdaBody lam
  params' <- mapM (rephraseParam $ rephraseLParamDec rephraser) $ lambdaParams lam
  return lam {lambdaBody = body', lambdaParams = params'}

mapper :: Monad m => Rephraser m from to -> Mapper from to m
mapper rephraser =
  identityMapper
    { mapOnBody = const $ rephraseBody rephraser,
      mapOnRetType = rephraseRetType rephraser,
      mapOnBranchType = rephraseBranchType rephraser,
      mapOnFParam = rephraseParam (rephraseFParamDec rephraser),
      mapOnLParam = rephraseParam (rephraseLParamDec rephraser),
      mapOnOp = rephraseOp rephraser
    }
