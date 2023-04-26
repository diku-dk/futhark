module Futhark.Internalise.FullNormalise (transformProg) where

import Control.Monad.State
import Data.Bifunctor
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Traversable
import Futhark.MonadFreshNames
import Language.Futhark
import Language.Futhark.Traversals

attributing :: [AttrInfo VName] -> Exp -> Exp
attributing attrs e =
  foldr f e attrs
  where
    f attr e' =
      Attr attr e' mempty

asserting :: [(Exp, Info T.Text, SrcLoc)] -> Exp -> Exp
asserting asss e =
  foldr f e asss
  where
    f (ass, txt, loc) e' =
      Assert ass e' txt loc

-- something to bind
data Binding
  = Ass Exp (Info T.Text) SrcLoc
  | PatBind Pat Exp
  | FunBind VName ([TypeParam], [Pat], Maybe (TypeExp Info VName), Info StructRetType, Exp)

-- state is the list of bindings, first to eval last
newtype OrderingM a = OrderingM (State ([Binding], VNameSource) a)
  deriving
    (Functor, Applicative, Monad)

instance MonadFreshNames OrderingM where
  getNameSource = OrderingM $ gets snd
  putNameSource = OrderingM . modify . second . const

addBind :: Binding -> OrderingM ()
addBind = OrderingM . modify . first . (:)

runOrdering :: MonadFreshNames m => OrderingM a -> m (a, [Binding])
runOrdering (OrderingM m) =
  modifyNameSource $ mod_tup . runState m . (mempty,)
  where
    mod_tup (a, (s, src)) = ((a, s), src)

nameExp :: Bool -> Exp -> OrderingM Exp
nameExp True e = pure e
nameExp False e = do
  name <- newNameFromString "tmp" -- "e<{" ++ prettyString e ++ "}>"
  let ty = typeOf e
      loc = srclocOf e
      pat = Id name (Info ty) loc
  addBind $ PatBind pat e
  pure $ Var (qualName name) (Info ty) loc

getOrdering :: Bool -> [AttrInfo VName] -> Exp -> OrderingM Exp
getOrdering final attrs (Attr attr e _) =
  getOrdering final (attr : attrs) e
getOrdering final attrs (Assert ass e txt loc) = do
  ass' <- getOrdering False attrs ass
  addBind $ Ass ass' txt loc
  getOrdering final attrs e
-- getOrdering (Update eb slice eu loc) ?
-- getOrdering (RecordUpdate eb ns eu ty loc) ?
getOrdering _ attrs (Lambda params body mte ret loc) = do
  body' <- transformBody attrs body
  pure $ attributing attrs (Lambda params body' mte ret loc)
-- getOrdering (OpSectionLeft, Right) ?
getOrdering final attrs (AppExp (Apply f args loc) resT) = do
  args' <- NE.reverse <$> mapM onArg (NE.reverse args)
  f' <- getOrdering False attrs f
  nameExp final $ attributing attrs (AppExp (Apply f' args' loc) resT)
  where
    onArg (d, e) = (d,) <$> getOrdering False attrs e
-- Coerce
getOrdering final attrs (AppExp (LetPat _ pat expr body _) _) = do
  expr' <- getOrdering True attrs expr
  addBind $ PatBind pat expr'
  getOrdering final attrs body
getOrdering final attrs (AppExp (LetFun vn (tparams, params, mrettype, rettype, body) e _) _) = do
  body' <- transformBody attrs body
  addBind $ FunBind vn (tparams, params, mrettype, rettype, body')
  getOrdering final attrs e
getOrdering final attrs (AppExp (If cond et ef loc) resT) = do
  cond' <- getOrdering True attrs cond
  et' <- transformBody attrs et
  ef' <- transformBody attrs ef
  nameExp final $ attributing attrs (AppExp (If cond' et' ef' loc) resT)
getOrdering final attrs (AppExp (DoLoop sizes pat einit form body loc) resT) = do
  einit' <- getOrdering False attrs einit
  form' <- case form of
    For ident e -> For ident <$> getOrdering True attrs e
    ForIn fpat e -> ForIn fpat <$> getOrdering True attrs e
    While e -> While <$> transformBody attrs e
  body' <- transformBody attrs body
  nameExp final $ attributing attrs (AppExp (DoLoop sizes pat einit' form' body' loc) resT)
getOrdering final attrs (AppExp (BinOp op opT (el, elT) (er, erT) loc) resT) = do
  expr' <- case (isOr, isAnd) of
    (True, _) -> do
      el' <- getOrdering True attrs el
      er' <- transformBody attrs er
      pure $ AppExp (If el' (Literal (BoolValue True) mempty) er' loc) resT
    (_, True) -> do
      el' <- getOrdering True attrs el
      er' <- transformBody attrs er
      pure $ AppExp (If el' er' (Literal (BoolValue False) mempty) loc) resT
    (False, False) -> do
      el' <- getOrdering False attrs el
      er' <- getOrdering False attrs er
      pure $ AppExp (BinOp op opT (el', elT) (er', erT) loc) resT
  nameExp final $ attributing attrs expr'
  where
    isOr = baseName (qualLeaf $ fst op) == "||"
    isAnd = baseName (qualLeaf $ fst op) == "&&"
getOrdering final attrs (AppExp (LetWith (Ident dest dty dloc) (Ident src sty sloc) slice e body loc) _) = do
  e' <- getOrdering False attrs e
  slice' <- astMap mapper slice
  addBind $ PatBind (Id dest dty dloc) (Update (Var (qualName src) sty sloc) slice' e' loc)
  getOrdering final attrs body
  where
    mapper = identityMapper {mapOnExp = getOrdering False attrs}
-- Index
getOrdering final attrs (AppExp (Match expr cs loc) resT) = do
  expr' <- getOrdering False attrs expr
  cs' <- mapM f cs
  nameExp final $ attributing attrs (AppExp (Match expr' cs' loc) resT)
  where
    f (CasePat pat body cloc) = do
      body' <- transformBody attrs body
      pure (CasePat pat body' cloc)
getOrdering final attrs (AppExp app resT) = do
  app' <- astMap mapper app
  nameExp final $ attributing attrs (AppExp app' resT)
  where
    -- types ?
    mapper = identityMapper {mapOnExp = getOrdering False attrs}
getOrdering _ attrs e = attributing attrs <$> astMap mapper e
  where
    -- types ?
    mapper = identityMapper {mapOnExp = getOrdering False attrs}

transformBody :: MonadFreshNames m => [AttrInfo VName] -> Exp -> m Exp
transformBody attrs e = do
  (e', pre_eval) <- runOrdering (getOrdering True attrs e)
  let (last_ass, pre_eval') = mapAccumR accum [] pre_eval
  pure $ foldl f (asserting last_ass e') pre_eval'
  where
    appRes = case e of
      (AppExp _ r) -> r
      _ -> Info $ AppRes (typeOf e) []

    accum acc b@(Ass ass txt loc) = ((ass, txt, loc) : acc, b)
    accum acc (PatBind p expr) = ([], PatBind p (asserting acc expr))
    accum acc b = (acc, b)
    f body Ass {} = body
    f body (PatBind p expr) =
      AppExp
        ( LetPat
            []
            p
            expr
            body
            mempty
        )
        appRes
    f body (FunBind vn infos) =
      AppExp (LetFun vn infos body mempty) appRes

transformDec :: MonadFreshNames m => Dec -> m Dec
transformDec (ValDec valbind) = do
  body' <- transformBody mempty $ valBindBody valbind
  pure $ ValDec (valbind {valBindBody = body'})
transformDec d = pure d

transformProg :: MonadFreshNames m => [Dec] -> m [Dec]
transformProg = mapM transformDec
