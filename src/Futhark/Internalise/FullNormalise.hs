module Futhark.Internalise.FullNormalise (transformProg) where

import Control.Monad.State
import Data.Bifunctor
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Traversable
import Futhark.MonadFreshNames
import Language.Futhark
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Types

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
  | Att (AttrInfo VName)
  | UnAtt
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

getOrdering :: Bool -> Exp -> OrderingM Exp
getOrdering _ e@Literal {} = pure e
getOrdering _ e@IntLit {} = pure e
getOrdering _ e@FloatLit {} = pure e
getOrdering _ e@StringLit {} = pure e
getOrdering _ e@Hole {} = pure e -- can we still have some ?
getOrdering _ e@Var {} = pure e
getOrdering _ (Parens e _) = getOrdering False e
getOrdering _ (QualParens _ e _) = getOrdering False e
getOrdering _ (TupLit es loc) = do
  es' <- mapM (getOrdering False) es
  pure $ TupLit es' loc
getOrdering _ (RecordLit fs loc) = do
  fs' <- mapM f fs
  pure $ RecordLit fs' loc
  where
    f (RecordFieldExplicit n e floc) = do
      e' <- getOrdering False e
      pure $ RecordFieldExplicit n e' floc
    f field@RecordFieldImplicit {} = pure field
getOrdering _ (ArrayLit es ty loc) = do
  es' <- mapM (getOrdering False) es
  pure $ ArrayLit es' ty loc
getOrdering final (Attr attr e loc) = do
  addBind $ Att attr
  e' <- getOrdering final e
  addBind UnAtt
  pure $ Attr attr e' loc
getOrdering _ (Project n e ty loc) = do
  e' <- getOrdering False e
  pure $ Project n e' ty loc
getOrdering _ (Negate e loc) = do
  e' <- getOrdering False e
  pure $ Negate e' loc
getOrdering _ (Not e loc) = do
  e' <- getOrdering False e
  pure $ Not e' loc
getOrdering final (Assert ass e txt loc) = do
  ass' <- getOrdering False ass
  addBind $ Ass ass' txt loc
  getOrdering final e
getOrdering final (Constr n es ty loc) = do
  es' <- mapM (getOrdering False) es
  nameExp final $ Constr n es' ty loc
getOrdering final (Update eb slice eu loc) = do
  eu' <- getOrdering False eu
  slice' <- astMap mapper slice
  eb' <- getOrdering False eb
  nameExp final $ Update eb' slice' eu' loc
  where
    mapper = identityMapper {mapOnExp = getOrdering False}
getOrdering final (RecordUpdate eb ns eu ty loc) = do
  eb' <- getOrdering False eb
  eu' <- getOrdering False eu
  nameExp final $ RecordUpdate eb' ns eu' ty loc
getOrdering final (Lambda params body mte ret loc) = do
  body' <- transformBody body
  nameExp final $ Lambda params body' mte ret loc
getOrdering _ (OpSection qn ty loc) =
  pure $ Var qn ty loc
getOrdering final (OpSectionLeft op ty e (Info (xp, _, xext), Info (yp, yty)) (Info (RetType dims ret), Info exts) loc) = do
  x <- getOrdering False e
  yn <- newNameFromString "y"
  let y = Var (qualName yn) (Info $ fromStruct yty) mempty
      ret' = applySubst (pSubst x y) ret
      body = mkApply (Var op ty mempty) [(Observe, xext, x), (Observe, Nothing, y)] $ AppRes ret' exts
  nameExp final $ Lambda [Id yn (Info $ fromStruct yty) mempty] body Nothing (Info (mempty, RetType dims $ toStruct ret')) loc
  where
    pSubst x y vn
      | Named p <- xp, p == vn = Just $ ExpSubst x
      | Named p <- yp, p == vn = Just $ ExpSubst y
      | otherwise = Nothing
getOrdering final (OpSectionRight op ty e (Info (xp, xty), Info (yp, _, yext)) (Info (RetType dims ret)) loc) = do
  xn <- newNameFromString "x"
  y <- getOrdering False e
  let x = Var (qualName xn) (Info $ fromStruct xty) mempty
      ret' = applySubst (pSubst x y) ret
      body = mkApply (Var op ty mempty) [(Observe, Nothing, x), (Observe, yext, y)] $ AppRes ret' []
  nameExp final $ Lambda [Id xn (Info $ fromStruct xty) mempty] body Nothing (Info (mempty, RetType dims $ toStruct ret')) loc
  where
    pSubst x y vn
      | Named p <- xp, p == vn = Just $ ExpSubst x
      | Named p <- yp, p == vn = Just $ ExpSubst y
      | otherwise = Nothing
getOrdering final (ProjectSection names (Info ty) loc) = do
  xn <- newNameFromString "x"
  let (xty, RetType dims ret) = case ty of
        Scalar (Arrow _ _ _ xty' ret') -> (xty', ret')
        _ -> error $ "not a function type for project section: " ++ prettyString ty
      x = Var (qualName xn) (Info $ fromStruct xty) mempty
      body = foldl project x names
  nameExp final $ Lambda [Id xn (Info $ fromStruct xty) mempty] body Nothing (Info (mempty, RetType dims $ toStruct ret)) loc
  where
    project e field =
      case typeOf e of
        Scalar (Record fs)
          | Just t <- M.lookup field fs ->
              Project field e (Info t) mempty
        t ->
          error $
            "desugar ProjectSection: type "
              ++ prettyString t
              ++ " does not have field "
              ++ prettyString field
getOrdering final (IndexSection slice (Info ty) loc) = do
  slice' <- astMap mapper slice
  xn <- newNameFromString "x"
  let (xty, RetType dims ret) = case ty of
        Scalar (Arrow _ _ _ xty' ret') -> (xty', ret')
        _ -> error $ "not a function type for index section: " ++ prettyString ty
      x = Var (qualName xn) (Info $ fromStruct xty) mempty
      body = AppExp (Index x slice' loc) (Info (AppRes ret []))
  nameExp final $ Lambda [Id xn (Info $ fromStruct xty) mempty] body Nothing (Info (mempty, RetType dims $ toStruct ret)) loc
  where
    mapper = identityMapper {mapOnExp = getOrdering False}
getOrdering _ (Ascript e _ _) = getOrdering False e
getOrdering final (AppExp (Apply f args loc) resT) = do
  args' <- NE.reverse <$> mapM onArg (NE.reverse args)
  f' <- getOrdering False f
  nameExp final $ AppExp (Apply f' args' loc) resT
  where
    onArg (d, e) = (d,) <$> getOrdering False e
getOrdering final (AppExp (Coerce e ty loc) resT) = do
  e' <- getOrdering False e
  nameExp final $ AppExp (Coerce e' ty loc) resT
getOrdering final (AppExp (Range start stride end loc) resT) = do
  start' <- getOrdering False start
  stride' <- mapM (getOrdering False) stride
  end' <- mapM (getOrdering False) end
  nameExp final $ AppExp (Range start' stride' end' loc) resT
getOrdering final (AppExp (LetPat _ pat expr body _) _) = do
  expr' <- getOrdering True expr
  addBind $ PatBind pat expr'
  getOrdering final body
getOrdering final (AppExp (LetFun vn (tparams, params, mrettype, rettype, body) e _) _) = do
  body' <- transformBody body
  addBind $ FunBind vn (tparams, params, mrettype, rettype, body')
  getOrdering final e
getOrdering final (AppExp (If cond et ef loc) resT) = do
  cond' <- getOrdering True cond
  et' <- transformBody et
  ef' <- transformBody ef
  nameExp final $ AppExp (If cond' et' ef' loc) resT
getOrdering final (AppExp (DoLoop sizes pat einit form body loc) resT) = do
  einit' <- getOrdering False einit
  form' <- case form of
    For ident e -> For ident <$> getOrdering True e
    ForIn fpat e -> ForIn fpat <$> getOrdering True e
    While e -> While <$> transformBody e
  body' <- transformBody body
  nameExp final $ AppExp (DoLoop sizes pat einit' form' body' loc) resT
getOrdering final (AppExp (BinOp (op, oloc) opT (el, Info (_, elp)) (er, Info (_, erp)) loc) (Info resT)) = do
  expr' <- case (isOr, isAnd) of
    (True, _) -> do
      el' <- getOrdering True el
      er' <- transformBody er
      pure $ AppExp (If el' (Literal (BoolValue True) mempty) er' loc) (Info resT)
    (_, True) -> do
      el' <- getOrdering True el
      er' <- transformBody er
      pure $ AppExp (If el' er' (Literal (BoolValue False) mempty) loc) (Info resT)
    (False, False) -> do
      el' <- getOrdering False el
      er' <- getOrdering False er
      pure $ mkApply (Var op opT oloc) [(Observe, elp, el'), (Observe, erp, er')] resT
  nameExp final expr'
  where
    isOr = baseName (qualLeaf op) == "||"
    isAnd = baseName (qualLeaf op) == "&&"
getOrdering final (AppExp (LetWith (Ident dest dty dloc) (Ident src sty sloc) slice e body loc) _) = do
  e' <- getOrdering False e
  slice' <- astMap mapper slice
  addBind $ PatBind (Id dest dty dloc) (Update (Var (qualName src) sty sloc) slice' e' loc)
  getOrdering final body
  where
    mapper = identityMapper {mapOnExp = getOrdering False}
getOrdering final (AppExp (Index e slice loc) resT) = do
  e' <- getOrdering False e
  slice' <- astMap mapper slice
  nameExp final $ AppExp (Index e' slice' loc) resT
  where
    mapper = identityMapper {mapOnExp = getOrdering False}
getOrdering final (AppExp (Match expr cs loc) resT) = do
  expr' <- getOrdering False expr
  cs' <- mapM f cs
  nameExp final $ AppExp (Match expr' cs' loc) resT
  where
    f (CasePat pat body cloc) = do
      body' <- transformBody body
      pure (CasePat pat body' cloc)

transformBody :: MonadFreshNames m => Exp -> m Exp
transformBody e = do
  (e', pre_eval) <- runOrdering (getOrdering True e)
  let ((last_ass, atts), pre_eval') = mapAccumR accum ([], []) pre_eval
  unless (null atts) $ pure $ error "not all attribute freed"
  pure $ foldl f (asserting last_ass e') pre_eval'
  where
    appRes = case e of
      (AppExp _ r) -> r
      _ -> Info $ AppRes (typeOf e) []

    accum (asss, atts) b@(Ass ass txt loc) = (((ass, txt, loc) : asss, atts), b)
    accum (asss, atts) b@(Att info) = ((asss, info : atts), b)
    accum (asss, atts) UnAtt = ((asss, tail atts), UnAtt)
    accum (asss, atts) (PatBind p expr) = (([], atts), PatBind p (attributing atts $ asserting asss expr))
    accum acc b = (acc, b)
    f body Ass {} = body
    f body Att {} = body
    f body UnAtt {} = body
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
  body' <- transformBody $ valBindBody valbind
  pure $ ValDec (valbind {valBindBody = body'})
transformDec d = pure d

transformProg :: MonadFreshNames m => [Dec] -> m [Dec]
transformProg = mapM transformDec
