-- | Facilities for changing the representation of some fragment,
-- within a monadic context.  We call this "rephrasing", for no deep
-- reason.
module Futhark.IR.Rephrase
  ( rephraseProg,
    rephraseFunDef,
    rephraseExp,
    rephraseBody,
    rephraseStm,
    rephraseLambda,
    rephrasePat,
    rephrasePatElem,
    Rephraser (..),
    RephraseOp (..),
  )
where

import Data.Bitraversable
import Futhark.IR.Syntax
import Futhark.IR.Traversals

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
rephraseProg :: (Monad m) => Rephraser m from to -> Prog from -> m (Prog to)
rephraseProg rephraser prog = do
  consts <- mapM (rephraseStm rephraser) (progConsts prog)
  funs <- mapM (rephraseFunDef rephraser) (progFuns prog)
  pure $ prog {progConsts = consts, progFuns = funs}

-- | Rephrase a function definition.
rephraseFunDef :: (Monad m) => Rephraser m from to -> FunDef from -> m (FunDef to)
rephraseFunDef rephraser fundec = do
  body' <- rephraseBody rephraser $ funDefBody fundec
  params' <- mapM (rephraseParam $ rephraseFParamDec rephraser) $ funDefParams fundec
  rettype' <- mapM (bitraverse (rephraseRetType rephraser) pure) $ funDefRetType fundec
  pure fundec {funDefBody = body', funDefParams = params', funDefRetType = rettype'}

-- | Rephrase an expression.
rephraseExp :: (Monad m) => Rephraser m from to -> Exp from -> m (Exp to)
rephraseExp = mapExpM . mapper

-- | Rephrase a statement.
rephraseStm :: (Monad m) => Rephraser m from to -> Stm from -> m (Stm to)
rephraseStm rephraser (Let pat aux e) =
  Let
    <$> rephrasePat (rephraseLetBoundDec rephraser) pat
    <*> traverse (rephraseExpDec rephraser) aux
    <*> rephraseExp rephraser e

-- | Rephrase a pattern.
rephrasePat ::
  (Monad m) =>
  (from -> m to) ->
  Pat from ->
  m (Pat to)
rephrasePat = traverse

-- | Rephrase a pattern element.
rephrasePatElem :: (Monad m) => (from -> m to) -> PatElem from -> m (PatElem to)
rephrasePatElem rephraser (PatElem ident from) =
  PatElem ident <$> rephraser from

-- | Rephrase a parameter.
rephraseParam :: (Monad m) => (from -> m to) -> Param from -> m (Param to)
rephraseParam rephraser (Param attrs name from) =
  Param attrs name <$> rephraser from

-- | Rephrase a body.
rephraseBody :: (Monad m) => Rephraser m from to -> Body from -> m (Body to)
rephraseBody rephraser (Body rep stms res) =
  Body
    <$> rephraseBodyDec rephraser rep
    <*> (stmsFromList <$> mapM (rephraseStm rephraser) (stmsToList stms))
    <*> pure res

-- | Rephrase a lambda.
rephraseLambda :: (Monad m) => Rephraser m from to -> Lambda from -> m (Lambda to)
rephraseLambda rephraser lam = do
  body' <- rephraseBody rephraser $ lambdaBody lam
  params' <- mapM (rephraseParam $ rephraseLParamDec rephraser) $ lambdaParams lam
  pure lam {lambdaBody = body', lambdaParams = params'}

mapper :: (Monad m) => Rephraser m from to -> Mapper from to m
mapper rephraser =
  identityMapper
    { mapOnBody = const $ rephraseBody rephraser,
      mapOnRetType = rephraseRetType rephraser,
      mapOnBranchType = rephraseBranchType rephraser,
      mapOnFParam = rephraseParam (rephraseFParamDec rephraser),
      mapOnLParam = rephraseParam (rephraseLParamDec rephraser),
      mapOnOp = rephraseOp rephraser
    }

-- | Rephrasing any fragments inside an Op from one representation to
-- another.
class RephraseOp op where
  rephraseInOp :: (Monad m) => Rephraser m from to -> op from -> m (op to)

instance RephraseOp NoOp where
  rephraseInOp _ NoOp = pure NoOp
