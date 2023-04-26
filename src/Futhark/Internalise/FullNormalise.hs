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
  | WithBind Ident Ident Slice Exp

-- state is the list of bindings, first to eval last
newtype OrderingM a = OrderingM (State ([Binding], VNameSource) a)
  deriving
    (Functor, Applicative, Monad)

instance MonadFreshNames OrderingM where
  getNameSource = OrderingM $ gets snd
  putNameSource = OrderingM . modify . second . const

instance MonadState [Binding] OrderingM where
  get = OrderingM $ gets fst
  put = OrderingM . modify . first . const

runOrdering :: MonadFreshNames m => OrderingM a -> m (a, [Binding])
runOrdering (OrderingM m) =
  modifyNameSource $ mod_tup . runState m . (mempty,)
  where
    mod_tup (a, (s, src)) = ((a, s), src)

nameExp :: Exp -> OrderingM Exp
nameExp e = do
  name <- newNameFromString "tmp" -- "e<{" ++ prettyString e ++ "}>"
  let ty = typeOf e
      loc = srclocOf e
      pat = Id name (Info ty) loc
  modify (PatBind pat e :)
  pure $ Var (qualName name) (Info ty) loc

getOrdering :: [AttrInfo VName] -> Exp -> OrderingM Exp
getOrdering attrs (Attr attr e _) =
  getOrdering (attr : attrs) e
getOrdering attrs (Assert ass e txt loc) = do
  ass' <- getOrdering attrs ass
  modify (Ass ass' txt loc :)
  getOrdering attrs e
-- getOrdering (Update eb slice eu loc) ?
-- getOrdering (RecordUpdate eb ns eu ty loc) ?
getOrdering attrs (Lambda params body mte ret loc) = do
  body' <- transformBody attrs body
  pure $ attributing attrs (Lambda params body' mte ret loc)
-- getOrdering (OpSectionLeft, Right) ?
getOrdering attrs (AppExp (Apply f args loc) resT) = do
  args' <- NE.reverse <$> mapM onArg (NE.reverse args)
  f' <- getOrdering attrs f
  nameExp $ attributing attrs (AppExp (Apply f' args' loc) resT)
  where
    onArg (d, e) = (d,) <$> getOrdering attrs e
-- Coerce
getOrdering attrs (AppExp (LetPat _ pat expr body _) _) = do
  expr' <- getOrdering attrs expr
  modify (PatBind pat expr' :)
  getOrdering attrs body
getOrdering attrs (AppExp (LetFun vn (tparams, params, mrettype, rettype, body) e _) _) = do
  body' <- transformBody attrs body
  modify (FunBind vn (tparams, params, mrettype, rettype, body') :)
  getOrdering attrs e
getOrdering attrs (AppExp (If cond et ef loc) resT) = do
  cond' <- getOrdering attrs cond
  et' <- transformBody attrs et
  ef' <- transformBody attrs ef
  nameExp $ attributing attrs (AppExp (If cond' et' ef' loc) resT)
getOrdering attrs (AppExp (DoLoop sizes pat einit form body loc) resT) = do
  einit' <- getOrdering attrs einit
  form' <- case form of
    For ident e -> For ident <$> getOrdering attrs e
    ForIn fpat e -> ForIn fpat <$> getOrdering attrs e
    While e -> While <$> transformBody attrs e
  body' <- transformBody attrs body
  nameExp $ attributing attrs (AppExp (DoLoop sizes pat einit' form' body' loc) resT)
getOrdering attrs (AppExp (BinOp op opT (el, elT) (er, erT) loc) resT) = do
  expr' <- case (isOr, isAnd) of
    (True, _) -> do
      el' <- getOrdering attrs el
      er' <- transformBody attrs er
      pure $ AppExp (If el' (Literal (BoolValue True) mempty) er' loc) resT
    (_, True) -> do
      el' <- getOrdering attrs el
      er' <- transformBody attrs er
      pure $ AppExp (If el' er' (Literal (BoolValue False) mempty) loc) resT
    (False, False) -> do
      el' <- getOrdering attrs el
      er' <- getOrdering attrs er
      pure $ AppExp (BinOp op opT (el', elT) (er', erT) loc) resT
  nameExp $ attributing attrs expr'
  where
    isOr = baseName (qualLeaf $ fst op) == "||"
    isAnd = baseName (qualLeaf $ fst op) == "&&"
getOrdering attrs (AppExp (LetWith ident1 ident2 slice e body _) _) = do
  e' <- getOrdering attrs e
  slice' <- astMap mapper slice
  modify (WithBind ident1 ident2 slice' e' :)
  getOrdering attrs body
  where
    mapper = identityMapper {mapOnExp = getOrdering attrs}
-- Index
getOrdering attrs (AppExp (Match expr cs loc) resT) = do
  expr' <- getOrdering attrs expr
  cs' <- mapM f cs
  nameExp $ attributing attrs (AppExp (Match expr' cs' loc) resT)
  where
    f (CasePat pat body cloc) = do
      body' <- transformBody attrs body
      pure (CasePat pat body' cloc)
getOrdering attrs (AppExp app resT) = do
  app' <- astMap mapper app
  nameExp $ attributing attrs (AppExp app' resT)
  where
    -- types ?
    mapper = identityMapper {mapOnExp = getOrdering attrs}
getOrdering attrs e = attributing attrs <$> astMap mapper e
  where
    -- types ?
    mapper = identityMapper {mapOnExp = getOrdering attrs}

transformBody :: MonadFreshNames m => [AttrInfo VName] -> Exp -> m Exp
transformBody attrs e = do
  (e', pre_eval) <- runOrdering (getOrdering attrs e)
  let (last_ass, pre_eval') = mapAccumR accum [] pre_eval
  pure $ foldl f (asserting last_ass e') pre_eval'
  where
    appRes = case e of
      (AppExp _ r) -> r
      _ -> Info $ AppRes (typeOf e) []

    accum acc b@(Ass ass txt loc) = ((ass, txt, loc) : acc, b)
    accum acc (PatBind p expr) = ([], PatBind p (asserting acc expr))
    accum acc b = (acc, b) -- todo
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
    f body (WithBind ident1 ident2 slice e') =
      AppExp (LetWith ident1 ident2 slice e' body mempty) appRes

transformDec :: MonadFreshNames m => Dec -> m Dec
transformDec (ValDec valbind) = do
  body' <- transformBody mempty $ valBindBody valbind
  pure $ ValDec (valbind {valBindBody = body'})
transformDec d = pure d

transformProg :: MonadFreshNames m => [Dec] -> m [Dec]
transformProg = mapM transformDec
