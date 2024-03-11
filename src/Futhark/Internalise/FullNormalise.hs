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
import Data.Functor.Identity
import Data.List (zip4)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Debug.Trace
import Futhark.MonadFreshNames
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.Pretty
import Language.Futhark.Primitive (intValue)
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
newtype OrderingM a = OrderingM (StateT NormState (Reader String) a)
  deriving
    (Functor, Applicative, Monad, MonadReader String, MonadState NormState)

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

naming :: String -> OrderingM a -> OrderingM a
naming s = local (const s)

-- | From now, we say an expression is "final" if it's going to be stored in a let-bind
-- or is at the end of the body e.g. after all lets

-- Replace a non-final expression by a let-binded variable
nameExp :: Bool -> Exp -> OrderingM Exp
nameExp True e = pure e
nameExp False e = do
  name <- newNameFromString =<< ask -- "e<{" ++ prettyString e ++ "}>"
  let ty = typeOf e
      loc = srclocOf e
      pat = Id name (Info ty) loc
  addBind $ PatBind [] pat e
  pure $ Var (qualName name) (Info ty) loc

-- An evocative name to use when naming subexpressions of the
-- expression bound to this pattern.
patRepName :: Pat t -> String
patRepName (PatAscription p _ _) = patRepName p
patRepName (Id v _ _) = baseString v
patRepName _ = "tmp"

expRepName :: Exp -> String
expRepName (Var v _ _) = prettyString v
expRepName e = "d<{" ++ prettyString (bareExp e) ++ "}>"

-- An evocative name to use when naming arguments to an application.
argRepName :: Exp -> Int -> String
argRepName e i = expRepName e <> "_arg" <> show i

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
    f (RecordFieldImplicit v t _) =
      f $ RecordFieldExplicit (baseName v) (Var (qualName v) t loc) loc
getOrdering _ (ArrayLit es ty loc) = do
  es' <- mapM (getOrdering False) es
  pure $ ArrayLit es' ty loc
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
getOrdering final (OpSectionLeft op ty e (Info (xp, _, xext, _), Info (yp, yty)) (Info (RetType dims ret), Info exts) loc) = do
  x <- getOrdering False e
  yn <- newNameFromString "y"
  let y = Var (qualName yn) (Info $ toStruct yty) mempty
      ret' = applySubst (pSubst x y) ret
      body =
        mkApply (Var op ty mempty) [(xext, mempty, x), (Nothing, mempty, y)] $
          AppRes (toStruct ret') exts
  nameExp final $ Lambda [Id yn (Info yty) mempty] body Nothing (Info (RetType dims ret')) loc
  where
    pSubst x y vn
      | Named p <- xp, p == vn = Just $ ExpSubst x
      | Named p <- yp, p == vn = Just $ ExpSubst y
      | otherwise = Nothing
getOrdering final (OpSectionRight op ty e (Info (xp, xty), Info (yp, _, yext, _)) (Info (RetType dims ret)) loc) = do
  xn <- newNameFromString "x"
  y <- getOrdering False e
  let x = Var (qualName xn) (Info $ toStruct xty) mempty
      ret' = applySubst (pSubst x y) ret
      body = mkApply (Var op ty mempty) [(Nothing, mempty, x), (yext, mempty, y)] $ AppRes (toStruct ret') []
  nameExp final $ Lambda [Id xn (Info xty) mempty] body Nothing (Info (RetType dims ret')) loc
  where
    pSubst x y vn
      | Named p <- xp, p == vn = Just $ ExpSubst x
      | Named p <- yp, p == vn = Just $ ExpSubst y
      | otherwise = Nothing
getOrdering final (ProjectSection names (Info ty) loc) = do
  xn <- newNameFromString "x"
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
  xn <- newNameFromString "x"
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
getOrdering final (AppExp (LetFun vn (tparams, params, mrettype, rettype, body) e _) _) = do
  body' <- transformBody body
  addBind $ FunBind vn (tparams, params, mrettype, rettype, body')
  getOrdering final e
getOrdering final (AppExp (If cond et ef loc) resT) = do
  cond' <- getOrdering True cond
  et' <- transformBody et
  ef' <- transformBody ef
  nameExp final $ AppExp (If cond' et' ef' loc) resT
getOrdering final (AppExp (Loop sizes pat einit form body loc) resT) = do
  einit' <- getOrdering False einit
  form' <- case form of
    For ident e -> For ident <$> getOrdering True e
    ForIn fpat e -> ForIn fpat <$> getOrdering True e
    While e -> While <$> transformBody e
  body' <- transformBody body
  nameExp final $ AppExp (Loop sizes pat einit' form' body' loc) resT
getOrdering final (AppExp (BinOp (op, oloc) opT (el, Info (elp, _)) (er, Info (erp, _)) loc) (Info resT)) = do
  -- Rewrite short-circuiting boolean operators on scalars to explicit
  -- if-then-else. Automapped cases are turned into applications of
  -- intrinsic functions.
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
      el' <- naming (prettyString op <> "_lhs") $ getOrdering False el
      er' <- naming (prettyString op <> "_rhs") $ getOrdering False er
      pure $ mkApply (Var op opT oloc) [(elp, mempty, el'), (erp, mempty, er')] resT
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
  (e', pre_eval) <- runOrdering $ getOrdering True e
  pure $ foldl f e' pre_eval
  where
    appRes = case e of
      (AppExp _ r) -> r
      _ -> Info $ AppRes (typeOf e) []

    f body (PatBind sizes p expr) =
      AppExp (LetPat sizes p expr body mempty) appRes
    f body (FunBind vn infos) =
      AppExp (LetFun vn infos body mempty) appRes

transformValBind :: (MonadFreshNames m) => ValBind -> m ValBind
transformValBind valbind = do
  body' <- transformBody <=< expandAMAnnotations $ valBindBody valbind
  pure $ valbind {valBindBody = body'}

transformProg :: (MonadFreshNames m) => [ValBind] -> m [ValBind]
transformProg = mapM transformValBind

--- | Expansion of 'AutoMap'-annotated applications.
---
--- Each application @f x@ has an annotation with @AutoMap R M F@ where
--- @R, M, F@ are the autorep, automap, and frame shapes,
--- respectively.
---
--- The application @f x@ will have type @F t@ for some @t@, i.e. @(f
--- x) : F t@. The frame @F@ is a prefix of the type of @f x@; namely
--- it is the total accumulated shape that is due to implicit maps.
--- Another way of thinking about that is that @|F|@ is is the level
--- of the automap-nest that @f x@ is in. For example, if @|F| = 2@
--- then we know that @f x@ implicitly stands for
---
--- > map (\x' -> map (\x'' -> f x'') x') x
---
--- For an application with a non-empty autorep annotation, the frame
--- tells about how many dimensions of the replicate can be eliminated.
--- For example, @[[1,2],[3,4]] + 5@ will yield the following annotations:
---
--- > ([[1,2],[3,4]] +)     -- AutoMap {R = mempty, M = [2][2], F = [2][2]}
--- > (([[1,2],[3,4]] +) 5) -- AutoMap {R = [2][2], M = mempty, F = [2][2]}
---
--- All replicated arguments are pushed down the auto-map nest. Each
--- time a replicated argument is pushed down a level of an
--- automap-nest, one fewer replicates is needed (i.e., the outermost
--- dimension of @R@ can be dropped). Replicated arguments are pushed
--- down the nest until either 1) the bottom of the nest is encountered
--- or 2) no replicate dimensions remain. For example, in the second
--- application above @R@ = @F@, so we can push the replicated argument
--- down two levels. Since each level effectively removes a dimension
--- of the replicate, no replicates will be required:
---
--- > map (\xs -> map (\x -> f x'' 5) xs) [[1,2],[3,4]]
---
--- The number of replicates that are actually required is given by
--- max(|R| - |F|, 0).
---
--- An expression's "true level" is the level at which that expression
--- will appear in the automap-nest. The bottom of a mapnest is level 0.
---
--- * For annotations with @R = mempty@, the true level is @|F|@.
--- * For annotations with @M = mempty@, the true level is @|F| - |R|@.
---
--- If @|R| > |F|@ then actual replicates (namely @|R| - |F|@ of them)
--- will be required at the bottom of the mapnest.
---
--- Note that replicates can only appear at the bottom of a mapnest; any
--- expression of the form
---
--- > map (\ls x' rs -> e) (replicate x)
---
--- can always be written as
---
--- > map (\ls rs -> e[x' -> x])
---
--- Let's look at another example. Consider (with exact sizes omitted for brevity)
---
--- > f    : a -> a -> a -> []a -> [][][]a -> a
--- > xss  : [][]a
--- > ys   : []a
--- > zsss : [][][]a
--- > w    : a
--- > vss  : [][]a
---
--- and the application
---
--- > f xss ys zsss w vss
---
--- which will have the following annotations
---
--- > (f xss)                          -- AutoMap {R = mempty,    M = [][],   F = [][]}    (1)
--- > ((f xss) ys)                     -- AutoMap {R = [],        M = mempty, F = [][]}    (2)
--- > (((f xss) ys) zsss)              -- AutoMap {R = mempty,    M = [],     F = [][][]}  (3)
--- > ((((f xss) ys) zsss) w)          -- AutoMap {R = [][][][],  M = mempty, F = [][][]}  (4)
--- > (((((f xss) ys) zsss) w) vss)    -- AutoMap {R = [],        M = mempty, F = [][][]}  (5)
---
--- This will yield the following mapnest.
---
--- >   map (\zss ->
--- >    map (\xs zs vs ->
--- >      map (\x y z v -> f x y z (replicate w) v) xs ys zs v) xss zss vss) zsss
---
--- Let's see how we'd construct this mapnest from the annotations. We construct
--- the nest bottom-up. We have:
---
--- Application | True level
--- ---------------------------
--- (1)         | |[][]|                = 2
--- (2)         | |[][]| - |[]|         = 1
--- (3)         | |[][][]|              = 3
--- (4)         | |[][][]| - |[][][][]| = -1
--- (5)         | |[][][]| - |[]|       = 2
---
--- We start at level 0.
--- * Any argument with a negative true level of @-n@ will be replicated @n@ times;
---   the exact shapes can be found by removing the @F@ postfix from @R@,
---   i.e. @R = shapes_to_rep_by <> F@.
--- * Any argument with a 0 true level will be included.
--- * For any argument @arg@ with a positive true level, we construct a new parameter
---   whose type is @arg@ with the leading @n@ dimensions (where @n@ is the true level)
---   removed.
---
--- Following the rules above, @w@ will be replicated once. For the remaining arguments,
--- we create new parameters @x : a, y : a, z : a , v : a@. Hence, level 0 becomes
---
--- > f x y z (replicate w) v
---
--- At level l > 0:
--- * There are no replicates.
--- * Any argument with l true level will be included verbatim.
--- * Any argument with true level > l will have a new parameter constructed for it,
---   whose type has the leading @n - l@ dimensions (where @n@ is the true level) removed.
--- * We surround the previous level with a map that binds that levels' new parameters
---   and is passed the current levels' arguments.
---
--- Following the above recipe for level 1, we create parameters
--- @xs : []a, zs : []a, vs :[]a@ and obtain
---
--- > map (\x y z v -> f x y z (replicate w) v) xs ys zs vs
---
--- This process continues until the level is greater than the maximum
--- true level of any application, at which we terminate.

-- | Expands 'AutoMap' annotations into explicit @map@s and @replicates@.
expandAMAnnotations :: (MonadFreshNames m) => Exp -> m Exp
expandAMAnnotations e = do
  case e of
    (AppExp (Apply f args loc) (Info res)) -> do
      let ((exts, ams), arg_es) = first unzip $ unzip $ map (first unInfo) $ NE.toList args
      f' <- expandAMAnnotations f
      arg_es' <- mapM expandAMAnnotations arg_es
      let diets = funDiets $ typeOf f
      withMapNest loc (zip4 exts ams arg_es' diets) $ \args' -> do
        let rettype =
              case unfoldFunTypeWithRet $ typeOf f' of
                Nothing -> error "Function type expected."
                Just (ptypes, f_ret) ->
                  foldFunType (drop (length args') ptypes) f_ret
        pure $
          mkApply f' (zip3 exts (repeat mempty) args') $
            res {appResType = rettype}
    (AppExp (BinOp op (Info t) (x, Info (xext, xam)) (y, Info (yext, yam)) loc) (Info res)) -> do
      x' <- expandAMAnnotations x
      y' <- expandAMAnnotations y
      withMapNest loc [(xext, xam, x', Observe), (yext, yam, y', Observe)] $ \[x'', y''] ->
        pure $
          AppExp
            ( BinOp
                op
                (Info t)
                (x'', Info (xext, mempty))
                (y'', Info (yext, mempty))
                loc
            )
            (Info res {appResType = stripArray (shapeRank $ autoFrame yam) (appResType res)})
    _ -> astMap identityMapper {mapOnExp = expandAMAnnotations} e
  where
    setNewType e t = astMap identityMapper {mapOnStructType = const $ pure t} e

    funDiets :: TypeBase dim as -> [Diet]
    funDiets (Scalar (Arrow _ _ d _ (RetType _ t2))) = d : funDiets t2
    funDiets _ = []

    dropDims :: Int -> TypeBase dim as -> TypeBase dim as
    dropDims n (Scalar (Arrow u p diet t1 (RetType ds t2))) =
      Scalar (Arrow u p diet (stripArray n t1) (RetType ds (dropDims n t2)))
    dropDims n t = stripArray n t

    innerFType :: TypeBase dim as -> [AutoMap] -> TypeBase dim as
    innerFType (Scalar (Arrow u p diet t1 (RetType ds t2))) ams =
      Scalar $ Arrow u p diet t1 $ RetType ds $ innerFType' t2 ams
      where
        innerFType' t [] = t
        innerFType' (Scalar (Arrow u p diet t1 (RetType ds t2))) (am : ams) =
          Scalar $ Arrow u p diet (dropDims (shapeRank (autoMap am)) t1) $ RetType ds $ innerFType' t2 ams
        innerFType' t [am] = dropDims (shapeRank (autoMap am)) t
        innerFType' _ _ = error ""
    innerFType _ _ = error ""

type Level = Int

data AutoMapArg = AutoMapArg
  { amArg :: Exp
  }
  deriving (Show)

data AutoMapParam = AutoMapParam
  { amParam :: Pat ParamType,
    amMapDim :: Size,
    amDiet :: Diet
  }
  deriving (Show)

-- | Builds a map-nest based on the 'AutoMap' annotations.
withMapNest ::
  forall m.
  (MonadFreshNames m) =>
  SrcLoc ->
  [(Maybe VName, AutoMap, Exp, Diet)] ->
  ([Exp] -> m Exp) ->
  m Exp
withMapNest loc args f = do
  (param_map, arg_map) <-
    bimap combineMaps combineMaps . unzip <$> mapM buildArgMap args
  buildMapNest param_map arg_map $ maximum $ M.keys arg_map
  where
    combineMaps :: (Ord k) => [M.Map k v] -> M.Map k [v]
    combineMaps = M.unionsWith (<>) . (fmap . fmap) pure

    buildMapNest ::
      M.Map Level [AutoMapParam] ->
      M.Map Level [AutoMapArg] ->
      Level ->
      m Exp
    buildMapNest _ arg_map 0 =
      f $ map amArg $ arg_map M.! 0
    buildMapNest param_map arg_map l =
      case map amMapDim $ param_map M.! l of
        [] -> error "Malformed param map."
        (map_dim : _) -> do
          let params = map (\p -> (amDiet p, amParam p)) $ param_map M.! l
              args = map amArg $ arg_map M.! l
          body <- buildMapNest param_map arg_map (l - 1)
          pure $
            mkMap map_dim params body args $
              RetType [] $
                arrayOfWithAliases Nonunique (Shape [map_dim]) (typeOf body)

    buildArgMap ::
      (Maybe VName, AutoMap, Exp, Diet) ->
      m (M.Map Level AutoMapParam, M.Map Level AutoMapArg)
    buildArgMap (ext, am, arg, diet) =
      foldM (mkArgsAndParams arg) mempty $ reverse [0 .. trueLevel am]
      where
        mkArgsAndParams arg (p_map, a_map) l
          | l == 0 = do
              let arg' = maybe arg (paramToExp . amParam) (p_map M.!? 1)
              rarg <- mkReplicateShape (autoRep am `shapePrefix` autoFrame am) arg'
              pure (p_map, M.insert 0 (AutoMapArg rarg) a_map)
          | l == trueLevel am = do
              p <- mkAMParam (typeOf arg) l
              let d = outerDim am l
              pure
                ( M.insert l (AutoMapParam p d diet) p_map,
                  M.insert l (AutoMapArg arg) a_map
                )
          | l < trueLevel am && l > 0 = do
              p <- mkAMParam (typeOf arg) l
              let d = outerDim am l
              let arg' =
                    paramToExp $
                      amParam $
                        p_map M.! (l + 1)
              pure
                ( M.insert l (AutoMapParam p d diet) p_map,
                  M.insert l (AutoMapArg arg') a_map
                )
          | otherwise = error "Impossible."

        mkAMParam t level =
          mkParam ("p_" <> show level) $ argType (level - 1) am t

    trueLevel :: AutoMap -> Int
    trueLevel am
      | autoMap am == mempty =
          max 0 $ shapeRank (autoFrame am) - shapeRank (autoRep am)
      | otherwise =
          shapeRank $ autoFrame am

    outerDim :: AutoMap -> Int -> Size
    outerDim am level =
      (!! (trueLevel am - level)) $ shapeDims $ autoFrame am

    argType level am = stripArray (trueLevel am - level)

mkParam :: (MonadFreshNames m) => String -> TypeBase Size u -> m (Pat ParamType)
mkParam desc t = do
  x <- newVName desc
  pure $ Id x (Info $ toParam Observe t) mempty

mkReplicateShape :: (MonadFreshNames m) => Shape Size -> Exp -> m Exp
mkReplicateShape s e = foldM (flip mkReplicate) e s

mkReplicate :: (MonadFreshNames m) => Exp -> Exp -> m Exp
mkReplicate dim e = do
  x <- mkParam "x" (Scalar $ Prim $ Unsigned Int64)
  pure $
    mkMap dim [(Observe, x)] e [xs] $
      RetType mempty (arrayOfWithAliases Unique (Shape [dim]) (typeOf e))
  where
    xs =
      AppExp
        ( Range
            (Literal (UnsignedValue $ intValue Int64 0) mempty)
            Nothing
            (UpToExclusive dim)
            mempty
        )
        ( Info $ AppRes (arrayOf (Shape [dim]) (Scalar $ Prim $ Unsigned Int64)) []
        )

mkMap :: Exp -> [(Diet, Pat ParamType)] -> Exp -> [Exp] -> ResRetType -> Exp
mkMap dim params body arrs rettype =
  mkApply mapN args (AppRes (toStruct $ retType rettype) [])
  where
    args = map (Nothing,mempty,) $ lambda : arrs
    mapt = foldFunType (zipWith toParam (Observe : map fst params) (typeOf lambda : map typeOf arrs)) rettype
    mapN = Var (QualName [] $ VName "map" 0) (Info mapt) mempty
    lambda =
      Lambda
        (map snd params)
        body
        Nothing
        ( Info $
            RetType
              (retDims rettype)
              (typeOf body `setUniqueness` uniqueness (retType rettype))
        )
        mempty

paramToExp :: Pat ParamType -> Exp
paramToExp (Id vn (Info t) loc) =
  Var (QualName [] vn) (Info $ toStruct t) loc
paramToExp p = error $ prettyString p
