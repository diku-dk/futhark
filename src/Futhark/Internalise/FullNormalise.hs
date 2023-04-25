module Futhark.Internalise.FullNormalise (transformProg) where

import Control.Monad.State
import Data.Bifunctor
import Data.List.NonEmpty qualified as NE
import Futhark.MonadFreshNames
import Language.Futhark
import Language.Futhark.Traversals

attributing :: [AttrInfo VName] -> Exp -> Exp
attributing attrs e =
  foldr f e attrs
  where
    f attr e' =
      Attr attr e' mempty

-- something to bind
data Binding
  = PatBind Pat Exp
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
  name <- newNameFromString $ "e<{" ++ prettyString e ++ "}>"
  let ty = typeOf e
      loc = srclocOf e
      pat = Id name (Info ty) loc
  modify (PatBind pat e :)
  pure $ Var (qualName name) (Info ty) loc

getOrdering :: Exp -> OrderingM Exp
getOrdering = getOrderingAttrs mempty

getOrderingAttrs :: [AttrInfo VName] -> Exp -> OrderingM Exp
getOrderingAttrs attrs e@(Var {}) =
  pure $ attributing attrs e
getOrderingAttrs attrs (Attr attr e _) = do
  e' <- getOrderingAttrs (attr : attrs) e
  case e' of
    Var {} -> pure e'
    _ -> pure $ attributing (attr : attrs) e'
getOrderingAttrs attrs (Assert ass e txt loc) = do
  ass' <- getOrdering ass
  modify
    ( PatBind
        (Wildcard (Info $ Scalar $ Prim $ Signed Int64) mempty)
        (Assert ass' (Literal (SignedValue $ Int64Value 0) mempty) txt loc)
        :
    )
  getOrderingAttrs attrs e
-- getOrdering (Update eb slice eu loc) ?
-- getOrdering (RecordUpdate eb ns eu ty loc) ?
getOrderingAttrs _ (Lambda params body mte ret loc) = do
  body' <- transformBody body
  pure (Lambda params body' mte ret loc)
-- getOrdering (OpSectionLeft, Right) ?
getOrderingAttrs attrs (AppExp (Apply f args loc) resT) = do
  args' <- NE.reverse <$> mapM onArg (NE.reverse args)
  f' <- getOrdering f
  nameExp $ attributing attrs (AppExp (Apply f' args' loc) resT)
  where
    onArg (d, e) = (d,) <$> getOrdering e
-- Coerce
getOrderingAttrs attrs (AppExp (LetPat _ pat expr body _) _) = do
  expr' <- getOrdering expr
  modify (PatBind pat expr' :)
  getOrderingAttrs attrs body
getOrderingAttrs attrs (AppExp (LetFun vn (tparams, params, mrettype, rettype, body) e _) _) = do
  body' <- transformBody body
  modify (FunBind vn (tparams, params, mrettype, rettype, body') :)
  getOrderingAttrs attrs e
getOrderingAttrs attrs (AppExp (If cond et ef loc) resT) = do
  cond' <- getOrdering cond
  et' <- transformBody et
  ef' <- transformBody ef
  nameExp $ attributing attrs (AppExp (If cond' et' ef' loc) resT)
getOrderingAttrs attrs (AppExp (DoLoop sizes pat einit form body loc) resT) = do
  einit' <- getOrdering einit
  form' <- case form of
    For ident e -> For ident <$> getOrdering e
    ForIn fpat e -> ForIn fpat <$> getOrdering e
    While e -> While <$> transformBody e
  body' <- transformBody body
  nameExp $ attributing attrs (AppExp (DoLoop sizes pat einit' form' body' loc) resT)
getOrderingAttrs attrs (AppExp (BinOp op opT (el, elT) (er, erT) loc) resT) = do
  el' <- getOrdering el
  er' <- getOrdering er
  nameExp $ attributing attrs (AppExp (BinOp op opT (el', elT) (er', erT) loc) resT)
getOrderingAttrs attrs (AppExp (LetWith ident1 ident2 slice e body _) _) = do
  e' <- getOrdering e
  slice' <- astMap mapper slice
  modify (WithBind ident1 ident2 slice' e' :)
  getOrderingAttrs attrs body
  where
    mapper = identityMapper {mapOnExp = getOrdering}
-- Index
getOrderingAttrs attrs (AppExp (Match expr cs loc) resT) = do
  expr' <- getOrdering expr
  cs' <- mapM f cs
  nameExp $ attributing attrs (AppExp (Match expr' cs' loc) resT)
  where
    f (CasePat pat body cloc) = do
      body' <- transformBody body
      pure (CasePat pat body' cloc)
getOrderingAttrs attrs (AppExp app resT) = do
  app' <- astMap mapper app
  nameExp $ attributing attrs (AppExp app' resT)
  where
    -- types ?
    mapper = identityMapper {mapOnExp = getOrdering}
getOrderingAttrs _ e = astMap mapper e
  where
    -- types ?
    mapper = identityMapper {mapOnExp = getOrdering}

transformBody :: MonadFreshNames m => Exp -> m Exp
transformBody e = do
  (e', pre_eval) <- runOrdering (getOrdering e)
  pure $ foldl f e' pre_eval
  where
    appRes = case e of
      (AppExp _ r) -> r
      _ -> Info $ AppRes (typeOf e) []
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
  body' <- transformBody $ valBindBody valbind
  pure $ ValDec (valbind {valBindBody = body'})
transformDec d = pure d

transformProg :: MonadFreshNames m => [Dec] -> m [Dec]
transformProg = mapM transformDec
