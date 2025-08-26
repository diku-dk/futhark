-- | This full normalisation module converts a well-typed, polymorphic,
-- module-free Futhark program into an equivalent with only simple expresssions.
-- Notably, all non-trivial expression are converted into a list of
-- let-bindings to make them simpler, with no nested apply, nested lets...
-- This module only performs syntactic operations.
--
-- Also, it performs various kinds of desugaring:
--
-- * Turns operator sections into explicit lambdas.
--
-- * Rewrites BinOp nodes to Apply nodes (&& and || are converted to conditionals).
--
-- * Turns `let x [i] = e1` into `let x = x with [i] = e1`.
--
-- * Binds all implicit sizes.
--
-- * Turns implicit record fields into explicit record fields.
--
-- This is currently not done for expressions inside sizes, this processing
-- still needed in monomorphisation for now.
module Futhark.Internalise.FullNormalise (transformProg) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.MonadFreshNames
import Futhark.Util (showText)
import Language.Futhark
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Types

-- Modifier to apply on binding, this is used to propagate attributes and move assertions
data BindModifier
  = Ass Exp (Info T.Text) SrcLoc
  | Att (AttrInfo VName)

-- Apply a list of modifiers, removing the assertions as it is not needed to check them multiple times
applyModifiers :: Exp -> [BindModifier] -> (Exp, [BindModifier])
applyModifiers =
  foldr f . (,[])
  where
    f (Ass ass txt loc) (body, modifs) =
      (Assert ass body txt loc, modifs)
    f (Att attr) (body, modifs) =
      (Attr attr body mempty, Att attr : modifs)

-- A binding that occurs in the calculation flow
data Binding
  = PatBind [SizeBinder VName] (Pat StructType) Exp
  | FunBind VName ([TypeParam], [Pat ParamType], Maybe (TypeExp Exp VName), Info ResRetType, Exp)

type NormState = (([Binding], [BindModifier]), VNameSource)

-- | Main monad of this module, the state as 3 parts:
-- * the VNameSource to produce new names
-- * the [Binding] is the accumulator for the result
--   It behave a bit like a writer
-- * the [BindModifier] is the current list of modifiers to apply to the introduced bindings
--   It behave like a reader for attributes modifier, and as a state for assertion,
--   they have to be in the same list to conserve their order
-- Direct interaction with the inside state should be done with caution, that's why their
-- no instance of `MonadState`.
newtype OrderingM a = OrderingM (StateT NormState (Reader Name) a)
  deriving
    (Functor, Applicative, Monad, MonadReader Name, MonadState NormState)

instance MonadFreshNames OrderingM where
  getNameSource = OrderingM $ gets snd
  putNameSource = OrderingM . modify . second . const

addModifier :: BindModifier -> OrderingM ()
addModifier = OrderingM . modify . first . second . (:)

rmModifier :: OrderingM ()
rmModifier = OrderingM $ modify $ first $ second tail

addBind :: Binding -> OrderingM ()
addBind (PatBind s p e) = do
  modifs <- gets $ snd . fst
  let (e', modifs') = applyModifiers e modifs
  modify $ first $ bimap (PatBind (s <> implicit) p e' :) (const modifs')
  where
    implicit = case e of
      (AppExp _ (Info (AppRes _ ext))) -> map (`SizeBinder` mempty) ext
      _ -> []
addBind b@FunBind {} =
  OrderingM $ modify $ first $ first (b :)

runOrdering :: (MonadFreshNames m) => OrderingM a -> m (a, [Binding])
runOrdering (OrderingM m) =
  modifyNameSource $ mod_tup . flip runReader "tmp" . runStateT m . (([], []),)
  where
    mod_tup (a, ((binds, modifs), src)) =
      if null modifs
        then ((a, binds), src)
        else error "not all bind modifiers were freed"

naming :: Name -> OrderingM a -> OrderingM a
naming s = local (const s)

-- | From now, we say an expression is "final" if it's going to be stored in a let-bind
-- or is at the end of the body e.g. after all lets

-- Replace a non-final expression by a let-binded variable
nameExp :: Bool -> Exp -> OrderingM Exp
nameExp True e = pure e
nameExp False e = do
  name <- newVName =<< ask -- "e<{" ++ prettyString e ++ "}>"
  let ty = typeOf e
      loc = srclocOf e
      pat = Id name (Info ty) loc
  addBind $ PatBind [] pat e
  pure $ Var (qualName name) (Info ty) loc

-- An evocative name to use when naming subexpressions of the
-- expression bound to this pattern.
patRepName :: Pat t -> Name
patRepName (PatAscription p _ _) = patRepName p
patRepName (Id v _ _) = baseName v
patRepName _ = "tmp"

expRepName :: Exp -> Name
expRepName (Var v _ _) = nameFromText $ prettyText v
expRepName e = "d<{" <> nameFromText (prettyText (bareExp e)) <> "}>"

-- An evocative name to use when naming arguments to an application.
argRepName :: Exp -> Int -> Name
argRepName e i = expRepName e <> "_arg" <> nameFromText (showText i)

-- Modify an expression as describe in module introduction,
-- introducing the let-bindings in the state.
getOrdering :: Bool -> Exp -> OrderingM Exp
getOrdering final (Assert ass e txt loc) = do
  ass' <- getOrdering False ass
  l_prev <- OrderingM $ gets $ length . snd . fst
  addModifier $ Ass ass' txt loc
  e' <- getOrdering final e
  l_after <- OrderingM $ gets $ length . snd . fst
  -- if the list of modifier has reduced in size, that means that
  -- all assertions as been inserted,
  -- else, we have to introduce the assertion ourself
  if l_after <= l_prev
    then pure e'
    else do
      rmModifier
      pure $ Assert ass' e' txt loc
getOrdering final (Attr attr e loc) = do
  -- propagate attribute
  addModifier $ Att attr
  e' <- getOrdering final e
  rmModifier
  pure $ Attr attr e' loc
getOrdering _ e@Literal {} = pure e
getOrdering _ e@IntLit {} = pure e
getOrdering _ e@FloatLit {} = pure e
getOrdering _ e@StringLit {} = pure e
getOrdering _ e@Hole {} = pure e -- can we still have some ?
getOrdering _ e@Var {} = pure e
getOrdering final (Parens e _) = getOrdering final e
getOrdering final (QualParens _ e _) = getOrdering final e
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
    f (RecordFieldImplicit (L vloc v) t _) =
      f $ RecordFieldExplicit (L vloc (baseName v)) (Var (qualName v) t loc) loc
getOrdering _ (ArrayVal vs t loc) =
  pure $ ArrayVal vs t loc
getOrdering _ (ArrayLit es ty loc)
  | Just vs <- mapM isLiteral es,
    Info (Array _ (Shape [_]) (Prim t)) <- ty =
      pure $ ArrayVal vs t loc
  | otherwise = do
      es' <- mapM (getOrdering False) es
      pure $ ArrayLit es' ty loc
  where
    isLiteral (Literal v _) = Just v
    isLiteral _ = Nothing
getOrdering _ (Project n e ty loc) = do
  e' <- getOrdering False e
  pure $ Project n e' ty loc
getOrdering _ (Negate e loc) = do
  e' <- getOrdering False e
  pure $ Negate e' loc
getOrdering _ (Not e loc) = do
  e' <- getOrdering False e
  pure $ Not e' loc
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
  yn <- newVName "y"
  let y = Var (qualName yn) (Info $ toStruct yty) mempty
      ret' = applySubst (pSubst x y) ret
      body =
        mkApply (Var op ty loc) [(xext, x), (Nothing, y)] $
          AppRes (toStruct ret') exts
  nameExp final $ Lambda [Id yn (Info yty) mempty] body Nothing (Info (RetType dims ret')) loc
  where
    pSubst x y vn
      | Named p <- xp, p == vn = Just $ ExpSubst x
      | Named p <- yp, p == vn = Just $ ExpSubst y
      | otherwise = Nothing
getOrdering final (OpSectionRight op ty e (Info (xp, xty), Info (yp, _, yext)) (Info (RetType dims ret)) loc) = do
  xn <- newVName "x"
  y <- getOrdering False e
  let x = Var (qualName xn) (Info $ toStruct xty) mempty
      ret' = applySubst (pSubst x y) ret
      body = mkApply (Var op ty loc) [(Nothing, x), (yext, y)] $ AppRes (toStruct ret') []
  nameExp final $ Lambda [Id xn (Info xty) mempty] body Nothing (Info (RetType dims ret')) loc
  where
    pSubst x y vn
      | Named p <- xp, p == vn = Just $ ExpSubst x
      | Named p <- yp, p == vn = Just $ ExpSubst y
      | otherwise = Nothing
getOrdering final (ProjectSection names (Info ty) loc) = do
  xn <- newVName "x"
  let (xty, RetType dims ret) = case ty of
        Scalar (Arrow _ _ d xty' ret') -> (toParam d xty', ret')
        _ -> error $ "not a function type for project section: " ++ prettyString ty
      x = Var (qualName xn) (Info $ toStruct xty) mempty
      body = foldl project x names
  nameExp final $ Lambda [Id xn (Info xty) mempty] body Nothing (Info (RetType dims ret)) loc
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
  xn <- newVName "x"
  let (xty, RetType dims ret) = case ty of
        Scalar (Arrow _ _ d xty' ret') -> (toParam d xty', ret')
        _ -> error $ "not a function type for index section: " ++ prettyString ty
      x = Var (qualName xn) (Info $ toStruct xty) mempty
      body = AppExp (Index x slice' loc) (Info (AppRes (toStruct ret) []))
  nameExp final $ Lambda [Id xn (Info xty) mempty] body Nothing (Info (RetType dims ret)) loc
  where
    mapper = identityMapper {mapOnExp = getOrdering False}
getOrdering _ (Ascript e _ _) = getOrdering False e
getOrdering final (AppExp (Apply f args loc) resT) = do
  args' <-
    NE.reverse <$> mapM onArg (NE.reverse (NE.zip args (NE.fromList [0 ..])))
  f' <- getOrdering False f
  nameExp final $ AppExp (Apply f' args' loc) resT
  where
    onArg ((d, e), i) =
      naming (argRepName f i) $ (d,) <$> getOrdering False e
getOrdering final (Coerce e te t loc) = do
  e' <- getOrdering False e
  nameExp final $ Coerce e' te t loc
getOrdering final (AppExp (Range start stride end loc) resT) = do
  start' <- getOrdering False start
  stride' <- mapM (getOrdering False) stride
  end' <- mapM (getOrdering False) end
  nameExp final $ AppExp (Range start' stride' end' loc) resT
getOrdering final (AppExp (LetPat sizes pat expr body _) _) = do
  expr' <- naming (patRepName pat) $ getOrdering True expr
  addBind $ PatBind sizes pat expr'
  getOrdering final body
getOrdering final (AppExp (LetFun (vn, _) (tparams, params, mrettype, rettype, body) e _) _) = do
  body' <- transformBody body
  addBind $ FunBind vn (tparams, params, mrettype, rettype, body')
  getOrdering final e
getOrdering final (AppExp (If cond et ef loc) resT) = do
  cond' <- getOrdering True cond
  et' <- transformBody et
  ef' <- transformBody ef
  nameExp final $ AppExp (If cond' et' ef' loc) resT
getOrdering final (AppExp (Loop sizes pat einit form body loc) resT) = do
  einit' <- getOrdering False $ loopInitExp einit
  form' <- case form of
    For ident e -> For ident <$> getOrdering True e
    ForIn fpat e -> ForIn fpat <$> getOrdering True e
    While e -> While <$> transformBody e
  body' <- transformBody body
  nameExp final $ AppExp (Loop sizes pat (LoopInitExplicit einit') form' body' loc) resT
getOrdering final (AppExp (BinOp (op, oloc) opT (el, Info elp) (er, Info erp) loc) (Info resT)) = do
  expr' <- case (isOr, isAnd) of
    (True, _) -> do
      el' <- naming "or_lhs" $ getOrdering True el
      er' <- naming "or_rhs" $ transformBody er
      pure $ AppExp (If el' (Literal (BoolValue True) mempty) er' loc) (Info resT)
    (_, True) -> do
      el' <- naming "and_lhs" $ getOrdering True el
      er' <- naming "and_rhs" $ transformBody er
      pure $ AppExp (If el' er' (Literal (BoolValue False) mempty) loc) (Info resT)
    (False, False) -> do
      el' <- naming (nameFromText (prettyText op) <> "_lhs") $ getOrdering False el
      er' <- naming (nameFromText (prettyText op) <> "_rhs") $ getOrdering False er
      pure $ mkApply (Var op opT oloc) [(elp, el'), (erp, er')] resT
  nameExp final expr'
  where
    isOr = baseName (qualLeaf op) == "||"
    isAnd = baseName (qualLeaf op) == "&&"
getOrdering final (AppExp (LetWith (Ident dest dty dloc) (Ident src sty sloc) slice e body loc) _) = do
  e' <- getOrdering False e
  slice' <- astMap mapper slice
  addBind $ PatBind [] (Id dest dty dloc) (Update (Var (qualName src) sty sloc) slice' e' loc)
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

-- Transform a body, e.g. the expression of a valbind,
-- branches of an if/match...
-- Note that this is not producing an OrderingM, produce
-- a complete separtion of states.
transformBody :: (MonadFreshNames m) => Exp -> m Exp
transformBody e = do
  (e', pre_eval) <- runOrdering (getOrdering True e)
  pure $ foldl f e' pre_eval
  where
    appRes = case e of
      (AppExp _ r) -> r
      _ -> Info $ AppRes (typeOf e) []

    f body (PatBind sizes p expr) =
      AppExp (LetPat sizes p expr body mempty) appRes
    f body (FunBind vn infos) =
      AppExp (LetFun (vn, mempty) infos body mempty) appRes

transformValBind :: (MonadFreshNames m) => ValBind -> m ValBind
transformValBind valbind = do
  body' <- transformBody $ valBindBody valbind
  pure $ valbind {valBindBody = body'}

-- | Fully normalise top level bindings.
transformProg :: (MonadFreshNames m) => [ValBind] -> m [ValBind]
transformProg = mapM transformValBind
