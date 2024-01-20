-- | Implementation of unification and other core type system building
-- blocks.
module Language.Futhark.TypeChecker.Unify
  ( Constraint (..),
    Usage (..),
    mkUsage,
    mkUsage',
    Level,
    Constraints,
    MonadUnify (..),
    Rigidity (..),
    RigidSource (..),
    BreadCrumbs,
    noBreadCrumbs,
    hasNoBreadCrumbs,
    dimNotes,
    zeroOrderType,
    arrayElemType,
    mustHaveConstr,
    mustHaveField,
    mustBeOneOf,
    equalityType,
    normType,
    normTypeFully,
    unify,
    unifyMostCommon,
    doUnification,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.List (foldl', intersect)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.TypeChecker.Monad hiding (BoundV)
import Language.Futhark.TypeChecker.Types

-- | A piece of information that describes what process the type
-- checker currently performing.  This is used to give better error
-- messages for unification errors.
data BreadCrumb
  = MatchingTypes StructType StructType
  | MatchingFields [Name]
  | MatchingConstructor Name
  | Matching (Doc ())

instance Pretty BreadCrumb where
  pretty (MatchingTypes t1 t2) =
    "When matching type"
      </> indent 2 (pretty t1)
      </> "with"
      </> indent 2 (pretty t2)
  pretty (MatchingFields fields) =
    "When matching types of record field"
      <+> dquotes (mconcat $ punctuate "." $ map pretty fields)
      <> dot
  pretty (MatchingConstructor c) =
    "When matching types of constructor" <+> dquotes (pretty c) <> dot
  pretty (Matching s) =
    unAnnotate s

-- | Unification failures can occur deep down inside complicated types
-- (consider nested records).  We leave breadcrumbs behind us so we
-- can report the path we took to find the mismatch.
newtype BreadCrumbs = BreadCrumbs [BreadCrumb]

-- | An empty path.
noBreadCrumbs :: BreadCrumbs
noBreadCrumbs = BreadCrumbs []

-- | Is the path empty?
hasNoBreadCrumbs :: BreadCrumbs -> Bool
hasNoBreadCrumbs (BreadCrumbs xs) = null xs

-- | Drop a breadcrumb on the path behind you.
breadCrumb :: BreadCrumb -> BreadCrumbs -> BreadCrumbs
breadCrumb (MatchingFields xs) (BreadCrumbs (MatchingFields ys : bcs)) =
  BreadCrumbs $ MatchingFields (ys ++ xs) : bcs
breadCrumb bc (BreadCrumbs bcs) =
  BreadCrumbs $ bc : bcs

instance Pretty BreadCrumbs where
  pretty (BreadCrumbs []) = mempty
  pretty (BreadCrumbs bcs) = line <> stack (map pretty bcs)

-- | A usage that caused a type constraint.
data Usage = Usage (Maybe T.Text) SrcLoc
  deriving (Show)

-- | Construct a 'Usage' from a location and a description.
mkUsage :: (Located a) => a -> T.Text -> Usage
mkUsage = flip (Usage . Just) . srclocOf

-- | Construct a 'Usage' that has just a location, but no particular
-- description.
mkUsage' :: (Located a) => a -> Usage
mkUsage' = Usage Nothing . srclocOf

instance Pretty Usage where
  pretty (Usage Nothing loc) = "use at " <> textwrap (locText loc)
  pretty (Usage (Just s) loc) = textwrap s <+> "at" <+> textwrap (locText loc)

instance Located Usage where
  locOf (Usage _ loc) = locOf loc

-- | The level at which a type variable is bound.  Higher means
-- deeper.  We can only unify a type variable at level @i@ with a type
-- @t@ if all type names that occur in @t@ are at most at level @i@.
type Level = Int

-- | A constraint on a yet-ambiguous type variable.
data Constraint
  = NoConstraint Liftedness Usage
  | ParamType Liftedness SrcLoc
  | Constraint StructRetType Usage
  | Overloaded [PrimType] Usage
  | HasFields Liftedness (M.Map Name StructType) Usage
  | Equality Usage
  | HasConstrs Liftedness (M.Map Name [StructType]) Usage
  | ParamSize SrcLoc
  | -- | Is not actually a type, but a term-level size,
    -- possibly already set to something specific.
    Size (Maybe Exp) Usage
  | -- | A size that does not unify with anything -
    -- created from the result of applying a function
    -- whose return size is existential, or otherwise
    -- hiding a size.
    UnknownSize SrcLoc RigidSource
  deriving (Show)

instance Located Constraint where
  locOf (NoConstraint _ usage) = locOf usage
  locOf (ParamType _ usage) = locOf usage
  locOf (Constraint _ usage) = locOf usage
  locOf (Overloaded _ usage) = locOf usage
  locOf (HasFields _ _ usage) = locOf usage
  locOf (Equality usage) = locOf usage
  locOf (HasConstrs _ _ usage) = locOf usage
  locOf (ParamSize loc) = locOf loc
  locOf (Size _ usage) = locOf usage
  locOf (UnknownSize loc _) = locOf loc

-- | Mapping from fresh type variables, instantiated from the type
-- schemes of polymorphic functions, to (possibly) specific types as
-- determined on application and the location of that application, or
-- a partial constraint on their type.
type Constraints = M.Map VName (Level, Constraint)

lookupSubst :: VName -> Constraints -> Maybe (Subst StructRetType)
lookupSubst v constraints = case snd <$> M.lookup v constraints of
  Just (Constraint t _) -> Just $ Subst [] $ applySubst (`lookupSubst` constraints) t
  Just (Size (Just d) _) ->
    Just $ ExpSubst $ applySubst (`lookupSubst` constraints) d
  _ -> Nothing

-- | The source of a rigid size.
data RigidSource
  = -- | A function argument that is not a constant or variable name.
    RigidArg (Maybe (QualName VName)) T.Text
  | -- | An existential return size.
    RigidRet (Maybe (QualName VName))
  | RigidLoop
  | -- | Produced by a complicated slice expression.
    RigidSlice (Maybe Size) T.Text
  | -- | Produced by a complicated range expression.
    RigidRange
  | -- | Produced by a range expression with this bound.
    RigidBound T.Text
  | -- | Mismatch in branches.
    RigidCond StructType StructType
  | -- | Invented during unification.
    RigidUnify
  | RigidOutOfScope SrcLoc VName
  | -- | Blank dimension in coercion.
    RigidCoerce
  deriving (Eq, Ord, Show)

-- | The ridigity of a size variable.  All rigid sizes are tagged with
-- information about how they were generated.
data Rigidity = Rigid RigidSource | Nonrigid
  deriving (Eq, Ord, Show)

prettySource :: SrcLoc -> SrcLoc -> RigidSource -> Doc ()
prettySource ctx loc (RigidRet Nothing) =
  "is unknown size returned by function at"
    <+> pretty (locStrRel ctx loc)
    <> "."
prettySource ctx loc (RigidRet (Just fname)) =
  "is unknown size returned by"
    <+> dquotes (pretty fname)
    <+> "at"
    <+> pretty (locStrRel ctx loc)
    <> "."
prettySource ctx loc (RigidArg fname arg) =
  "is value of argument"
    </> indent 2 (shorten (pretty arg))
    </> "passed to"
    <+> fname'
    <+> "at"
    <+> pretty (locStrRel ctx loc)
    <> "."
  where
    fname' = maybe "function" (dquotes . pretty) fname
prettySource ctx loc (RigidSlice d slice) =
  "is size produced by slice"
    </> indent 2 (shorten (pretty slice))
    </> d_desc
    <> "at"
      <+> pretty (locStrRel ctx loc)
    <> "."
  where
    d_desc = case d of
      Just d' -> "of dimension of size " <> dquotes (pretty d') <> " "
      Nothing -> mempty
prettySource ctx loc RigidLoop =
  "is unknown size of value returned at" <+> pretty (locStrRel ctx loc) <> "."
prettySource ctx loc RigidRange =
  "is unknown length of range at" <+> pretty (locStrRel ctx loc) <> "."
prettySource ctx loc (RigidBound bound) =
  "generated from expression"
    </> indent 2 (shorten (pretty bound))
    </> "used in range at "
    <> pretty (locStrRel ctx loc)
    <> "."
prettySource ctx loc (RigidOutOfScope boundloc v) =
  "is an unknown size arising from "
    <> dquotes (prettyName v)
    <> " going out of scope at "
    <> pretty (locStrRel ctx loc)
    <> "."
      </> "Originally bound at "
    <> pretty (locStrRel ctx boundloc)
    <> "."
prettySource ctx loc RigidCoerce =
  "is an unknown size arising from empty dimension in coercion at"
    <+> pretty (locStrRel ctx loc)
    <> "."
prettySource _ _ RigidUnify =
  "is an artificial size invented during unification of functions with anonymous sizes."
prettySource ctx loc (RigidCond t1 t2) =
  "is unknown due to conditional expression at "
    <> pretty (locStrRel ctx loc)
    <> "."
      </> "One branch returns array of type: "
    <> align (pretty t1)
      </> "The other an array of type:       "
    <> align (pretty t2)

-- | Retrieve notes describing the purpose or origin of the given
-- t'Size'.  The location is used as the *current* location, for the
-- purpose of reporting relative locations.
dimNotes :: (Located a, MonadUnify m) => a -> Exp -> m Notes
dimNotes ctx (Var d _ _) = do
  c <- M.lookup (qualLeaf d) <$> getConstraints
  case c of
    Just (_, UnknownSize loc rsrc) ->
      pure . aNote $
        dquotes (pretty d) <+> prettySource (srclocOf ctx) loc rsrc
    _ -> pure mempty
dimNotes _ _ = pure mempty

typeNotes :: (Located a, MonadUnify m) => a -> StructType -> m Notes
typeNotes ctx =
  fmap mconcat
    . mapM (dimNotes ctx . flip sizeFromName mempty . qualName)
    . S.toList
    . fvVars
    . freeInType

typeVarNotes :: (MonadUnify m) => VName -> m Notes
typeVarNotes v = maybe mempty (note . snd) . M.lookup v <$> getConstraints
  where
    note (HasConstrs _ cs _) =
      aNote $
        prettyName v
          <+> "="
          <+> hsep (map ppConstr (M.toList cs))
          <+> "..."
    note (Overloaded ts _) =
      aNote $ prettyName v <+> "must be one of" <+> mconcat (punctuate ", " (map pretty ts))
    note (HasFields _ fs _) =
      aNote $
        prettyName v
          <+> "="
          <+> braces (mconcat (punctuate ", " (map ppField (M.toList fs))))
    note _ = mempty

    ppConstr (c, _) = "#" <> pretty c <+> "..." <+> "|"
    ppField (f, _) = prettyName f <> ":" <+> "..."

-- | Monads that which to perform unification must implement this type
-- class.
class (Monad m) => MonadUnify m where
  getConstraints :: m Constraints
  putConstraints :: Constraints -> m ()
  modifyConstraints :: (Constraints -> Constraints) -> m ()
  modifyConstraints f = do
    x <- getConstraints
    putConstraints $ f x

  newTypeVar :: (Monoid als) => SrcLoc -> Name -> m (TypeBase dim als)
  newDimVar :: Usage -> Rigidity -> Name -> m VName
  newRigidDim :: (Located a) => a -> RigidSource -> Name -> m VName
  newRigidDim loc = newDimVar (mkUsage' loc) . Rigid
  newFlexibleDim :: Usage -> Name -> m VName
  newFlexibleDim usage = newDimVar usage Nonrigid

  curLevel :: m Level

  matchError ::
    (Located loc) =>
    loc ->
    Notes ->
    BreadCrumbs ->
    StructType ->
    StructType ->
    m a

  unifyError ::
    (Located loc) =>
    loc ->
    Notes ->
    BreadCrumbs ->
    Doc () ->
    m a

-- | Replace all type variables with their substitution.
normTypeFully :: (Substitutable a, MonadUnify m) => a -> m a
normTypeFully t = do
  constraints <- getConstraints
  pure $ applySubst (`lookupSubst` constraints) t

-- | Replace any top-level type variable with its substitution.
normType :: (MonadUnify m) => StructType -> m StructType
normType t@(Scalar (TypeVar _ (QualName [] v) [])) = do
  constraints <- getConstraints
  case snd <$> M.lookup v constraints of
    Just (Constraint (RetType [] t') _) -> normType t'
    _ -> pure t
normType t = pure t

rigidConstraint :: Constraint -> Bool
rigidConstraint ParamType {} = True
rigidConstraint ParamSize {} = True
rigidConstraint UnknownSize {} = True
rigidConstraint _ = False

unsharedConstructorsMsg :: M.Map Name t -> M.Map Name t -> Doc a
unsharedConstructorsMsg cs1 cs2 =
  "Unshared constructors:" <+> commasep (map (("#" <>) . pretty) missing) <> "."
  where
    missing =
      filter (`notElem` M.keys cs1) (M.keys cs2)
        ++ filter (`notElem` M.keys cs2) (M.keys cs1)

-- | Is the given type variable the name of an abstract type or type
-- parameter, which we cannot substitute?
isRigid :: VName -> Constraints -> Bool
isRigid v constraints =
  maybe True (rigidConstraint . snd) $ M.lookup v constraints

-- | If the given type variable is nonrigid, what is its level?
isNonRigid :: VName -> Constraints -> Maybe Level
isNonRigid v constraints = do
  (lvl, c) <- M.lookup v constraints
  guard $ not $ rigidConstraint c
  pure lvl

type UnifySizes m =
  BreadCrumbs -> [VName] -> (VName -> Maybe Int) -> Exp -> Exp -> m ()

flipUnifySizes :: UnifySizes m -> UnifySizes m
flipUnifySizes onDims bcs bound nonrigid t1 t2 =
  onDims bcs bound nonrigid t2 t1

unifyWith ::
  (MonadUnify m) =>
  UnifySizes m ->
  Usage ->
  [VName] ->
  BreadCrumbs ->
  StructType ->
  StructType ->
  m ()
unifyWith onDims usage = subunify False
  where
    swap True x y = (y, x)
    swap False x y = (x, y)

    subunify ord bound bcs t1 t2 = do
      constraints <- getConstraints

      t1' <- normType t1
      t2' <- normType t2

      let nonrigid v = isNonRigid v constraints

          failure = matchError (srclocOf usage) mempty bcs t1' t2'

          link ord' =
            linkVarToType linkDims usage bound bcs
            where
              -- We may have to flip the order of future calls to
              -- onDims inside linkVarToType.
              linkDims
                | ord' = flipUnifySizes onDims
                | otherwise = onDims

          unifyTypeArg bcs' (TypeArgDim d1) (TypeArgDim d2) =
            onDims' bcs' (swap ord d1 d2)
          unifyTypeArg bcs' (TypeArgType t) (TypeArgType arg_t) =
            subunify ord bound bcs' t arg_t
          unifyTypeArg bcs' _ _ =
            unifyError
              usage
              mempty
              bcs'
              "Cannot unify a type argument with a dimension argument (or vice versa)."

          onDims' bcs' (d1, d2) =
            onDims
              bcs'
              bound
              nonrigid
              (applySubst (`lookupSubst` constraints) d1)
              (applySubst (`lookupSubst` constraints) d2)

      case (t1', t2') of
        (Scalar (Prim pt1), Scalar (Prim pt2))
          | pt1 == pt2 -> pure ()
        ( Scalar (Record fs),
          Scalar (Record arg_fs)
          )
            | M.keys fs == M.keys arg_fs ->
                unifySharedFields onDims usage bound bcs fs arg_fs
            | otherwise -> do
                let missing =
                      filter (`notElem` M.keys arg_fs) (M.keys fs)
                        ++ filter (`notElem` M.keys fs) (M.keys arg_fs)
                unifyError usage mempty bcs $
                  "Unshared fields:" <+> commasep (map pretty missing) <> "."
        ( Scalar (TypeVar _ (QualName _ tn) targs),
          Scalar (TypeVar _ (QualName _ arg_tn) arg_targs)
          )
            | tn == arg_tn,
              length targs == length arg_targs -> do
                let bcs' = breadCrumb (Matching "When matching type arguments.") bcs
                zipWithM_ (unifyTypeArg bcs') targs arg_targs
        ( Scalar (TypeVar _ (QualName [] v1) []),
          Scalar (TypeVar _ (QualName [] v2) [])
          ) ->
            case (nonrigid v1, nonrigid v2) of
              (Nothing, Nothing) -> failure
              (Just lvl1, Nothing) -> link ord v1 lvl1 t2'
              (Nothing, Just lvl2) -> link (not ord) v2 lvl2 t1'
              (Just lvl1, Just lvl2)
                | lvl1 <= lvl2 -> link ord v1 lvl1 t2'
                | otherwise -> link (not ord) v2 lvl2 t1'
        (Scalar (TypeVar _ (QualName [] v1) []), _)
          | Just lvl <- nonrigid v1 ->
              link ord v1 lvl t2'
        (_, Scalar (TypeVar _ (QualName [] v2) []))
          | Just lvl <- nonrigid v2 ->
              link (not ord) v2 lvl t1'
        ( Scalar (Arrow _ p1 d1 a1 (RetType b1_dims b1)),
          Scalar (Arrow _ p2 d2 a2 (RetType b2_dims b2))
          )
            | uncurry (<) $ swap ord d1 d2 -> do
                unifyError usage mempty bcs . withIndexLink "unify-consuming-param" $
                  "Parameters"
                    </> indent 2 (pretty d1 <> pretty a1)
                    </> "and"
                    </> indent 2 (pretty d2 <> pretty a2)
                    </> "are incompatible regarding consuming their arguments."
            | uncurry (<) $ swap ord (uniqueness b2) (uniqueness b1) -> do
                unifyError usage mempty bcs . withIndexLink "unify-return-uniqueness" $
                  "Return types"
                    </> indent 2 (pretty d1 <> pretty b1)
                    </> "and"
                    </> indent 2 (pretty d2 <> pretty b2)
                    </> "have incompatible uniqueness."
            | otherwise -> do
                -- Introduce the existentials as size variables so they
                -- are subject to unification.  We will remove them again
                -- afterwards.
                let (r1, r2) =
                      swap
                        ord
                        (Size Nothing $ Usage Nothing mempty)
                        (UnknownSize mempty RigidUnify)
                lvl <- curLevel
                modifyConstraints (M.fromList (map (,(lvl, r1)) b1_dims) <>)
                modifyConstraints (M.fromList (map (,(lvl, r2)) b2_dims) <>)

                let bound' = bound <> mapMaybe pname [p1, p2] <> b1_dims <> b2_dims
                subunify
                  (not ord)
                  bound
                  (breadCrumb (Matching "When matching parameter types.") bcs)
                  a1
                  a2
                subunify
                  ord
                  bound'
                  (breadCrumb (Matching "When matching return types.") bcs)
                  (toStruct b1')
                  (toStruct b2')

                -- Delete the size variables we introduced to represent
                -- the existential sizes.
                modifyConstraints $ \m -> foldl' (flip M.delete) m (b1_dims <> b2_dims)
            where
              (b1', b2') =
                -- Replace one parameter name with the other in the
                -- return type, in case of dependent types.  I.e.,
                -- we want type '(n: i32) -> [n]i32' to unify with
                -- type '(x: i32) -> [x]i32'.
                case (p1, p2) of
                  (Named p1', Named p2') ->
                    let f v
                          | v == p2' = Just $ ExpSubst $ sizeFromName (qualName p1') mempty
                          | otherwise = Nothing
                     in (b1, applySubst f b2)
                  (_, _) ->
                    (b1, b2)

              pname (Named x) = Just x
              pname Unnamed = Nothing
        (Array {}, Array {})
          | Shape (t1_d : _) <- arrayShape t1',
            Shape (t2_d : _) <- arrayShape t2',
            Just t1'' <- peelArray 1 t1',
            Just t2'' <- peelArray 1 t2' -> do
              onDims' bcs (swap ord t1_d t2_d)
              subunify ord bound bcs t1'' t2''
        ( Scalar (Sum cs),
          Scalar (Sum arg_cs)
          )
            | M.keys cs == M.keys arg_cs ->
                unifySharedConstructors onDims usage bound bcs cs arg_cs
            | otherwise ->
                unifyError usage mempty bcs $ unsharedConstructorsMsg arg_cs cs
        _ -> failure

anyBound :: [VName] -> ExpBase Info VName -> Bool
anyBound bound e = any (`S.member` fvVars (freeInExp e)) bound

unifySizes :: (MonadUnify m) => Usage -> UnifySizes m
unifySizes usage bcs bound nonrigid e1 e2
  | Just es <- similarExps e1 e2 =
      mapM_ (uncurry $ unifySizes usage bcs bound nonrigid) es
unifySizes usage bcs bound nonrigid (Var v1 _ _) e2
  | Just lvl1 <- nonrigid (qualLeaf v1),
    not (anyBound bound e2) || (qualLeaf v1 `elem` bound) =
      linkVarToDim usage bcs (qualLeaf v1) lvl1 e2
unifySizes usage bcs bound nonrigid e1 (Var v2 _ _)
  | Just lvl2 <- nonrigid (qualLeaf v2),
    not (anyBound bound e1) || (qualLeaf v2 `elem` bound) =
      linkVarToDim usage bcs (qualLeaf v2) lvl2 e1
unifySizes usage bcs _ _ e1 e2 = do
  notes <- (<>) <$> dimNotes usage e2 <*> dimNotes usage e2
  unifyError usage notes bcs $
    "Sizes"
      <+> dquotes (pretty e1)
      <+> "and"
      <+> dquotes (pretty e2)
      <+> "do not match."

-- | Unifies two types.
unify :: (MonadUnify m) => Usage -> StructType -> StructType -> m ()
unify usage = unifyWith (unifySizes usage) usage mempty noBreadCrumbs

occursCheck ::
  (MonadUnify m) =>
  Usage ->
  BreadCrumbs ->
  VName ->
  StructType ->
  m ()
occursCheck usage bcs vn tp =
  when (vn `S.member` typeVars tp) $
    unifyError usage mempty bcs $
      "Occurs check: cannot instantiate"
        <+> prettyName vn
        <+> "with"
        <+> pretty tp
        <> "."

scopeCheck ::
  (MonadUnify m) =>
  Usage ->
  BreadCrumbs ->
  VName ->
  Level ->
  StructType ->
  m ()
scopeCheck usage bcs vn max_lvl tp = do
  constraints <- getConstraints
  checkType constraints tp
  where
    checkType constraints t =
      mapM_ (check constraints) $ typeVars t <> fvVars (freeInType t)

    check constraints v
      | Just (lvl, c) <- M.lookup v constraints,
        lvl > max_lvl =
          if rigidConstraint c
            then scopeViolation v
            else modifyConstraints $ M.insert v (max_lvl, c)
      | otherwise =
          pure ()

    scopeViolation v = do
      notes <- typeNotes usage tp
      unifyError usage notes bcs $
        "Cannot unify type"
          </> indent 2 (pretty tp)
          </> "with"
          <+> dquotes (prettyName vn)
          <+> "(scope violation)."
          </> "This is because"
          <+> dquotes (prettyName v)
          <+> "is rigidly bound in a deeper scope."

linkVarToType ::
  (MonadUnify m) =>
  UnifySizes m ->
  Usage ->
  [VName] ->
  BreadCrumbs ->
  VName ->
  Level ->
  StructType ->
  m ()
linkVarToType onDims usage bound bcs vn lvl tp_unnorm = do
  -- We have to expand anyway for the occurs check, so we might as
  -- well link the fully expanded type.
  tp <- normTypeFully tp_unnorm
  occursCheck usage bcs vn tp
  scopeCheck usage bcs vn lvl tp

  constraints <- getConstraints
  let link = do
        let (witnessed, not_witnessed) = determineSizeWitnesses tp
            used v = v `S.member` witnessed || v `S.member` not_witnessed
            ext = filter used bound
        case filter (`notElem` witnessed) ext of
          [] ->
            modifyConstraints $
              M.insert vn (lvl, Constraint (RetType ext tp) usage)
          problems ->
            unifyError usage mempty bcs . withIndexLink "unify-param-existential" $
              "Parameter(s) "
                <> commasep (map (dquotes . prettyName) problems)
                <> " used as size(s) would go out of scope."

  let unliftedBcs unlifted_usage =
        breadCrumb
          ( Matching $
              "When verifying that"
                <+> dquotes (prettyName vn)
                <+> textwrap "is not instantiated with a function type, due to"
                <+> pretty unlifted_usage
          )
          bcs

  case snd <$> M.lookup vn constraints of
    Just (NoConstraint Unlifted unlift_usage) -> do
      link

      arrayElemTypeWith usage (unliftedBcs unlift_usage) tp
      when (any (`elem` bound) (fvVars (freeInType tp))) $
        unifyError usage mempty bcs $
          "Type variable"
            <+> prettyName vn
            <+> "cannot be instantiated with type containing anonymous sizes:"
            </> indent 2 (pretty tp)
            </> textwrap "This is usually because the size of an array returned by a higher-order function argument cannot be determined statically.  This can also be due to the return size being a value parameter.  Add type annotation to clarify."
    Just (Equality _) -> do
      link
      equalityType usage tp
    Just (Overloaded ts old_usage)
      | tp `notElem` map (Scalar . Prim) ts -> do
          link
          case tp of
            Scalar (TypeVar _ (QualName [] v) [])
              | not $ isRigid v constraints ->
                  linkVarToTypes usage v ts
            _ ->
              unifyError usage mempty bcs $
                "Cannot instantiate"
                  <+> dquotes (prettyName vn)
                  <+> "with type"
                  </> indent 2 (pretty tp)
                  </> "as"
                  <+> dquotes (prettyName vn)
                  <+> "must be one of"
                  <+> commasep (map pretty ts)
                  </> "due to"
                  <+> pretty old_usage
                  <> "."
    Just (HasFields l required_fields old_usage) -> do
      when (l == Unlifted) $ arrayElemTypeWith usage (unliftedBcs old_usage) tp
      case tp of
        Scalar (Record tp_fields)
          | all (`M.member` tp_fields) $ M.keys required_fields -> do
              required_fields' <- mapM normTypeFully required_fields
              let tp' = Scalar $ Record $ required_fields <> tp_fields -- Crucially left-biased.
                  ext = filter (`S.member` fvVars (freeInType tp')) bound
              modifyConstraints $
                M.insert vn (lvl, Constraint (RetType ext tp') usage)
              unifySharedFields onDims usage bound bcs required_fields' tp_fields
        Scalar (TypeVar _ (QualName [] v) []) -> do
          case M.lookup v constraints of
            Just (_, HasFields _ tp_fields _) ->
              unifySharedFields onDims usage bound bcs required_fields tp_fields
            Just (_, NoConstraint {}) -> pure ()
            Just (_, Equality {}) -> pure ()
            _ -> do
              notes <- (<>) <$> typeVarNotes vn <*> typeVarNotes v
              noRecordType notes
          link
          modifyConstraints $
            M.insertWith
              combineFields
              v
              (lvl, HasFields l required_fields old_usage)
          where
            combineFields (_, HasFields l1 fs1 usage1) (_, HasFields l2 fs2 _) =
              (lvl, HasFields (l1 `min` l2) (M.union fs1 fs2) usage1)
            combineFields hasfs _ = hasfs
        _ ->
          unifyError usage mempty bcs $
            "Cannot instantiate"
              <+> dquotes (prettyName vn)
              <+> "with type"
              </> indent 2 (pretty tp)
              </> "as"
              <+> dquotes (prettyName vn)
              <+> "must be a record with fields"
              </> indent 2 (pretty (Record required_fields))
              </> "due to"
              <+> pretty old_usage
              <> "."
    -- See Note [Linking variables to sum types]
    Just (HasConstrs l required_cs old_usage) -> do
      when (l == Unlifted) $ arrayElemTypeWith usage (unliftedBcs old_usage) tp
      case tp of
        Scalar (Sum ts)
          | all (`M.member` ts) $ M.keys required_cs -> do
              let tp' = Scalar $ Sum $ required_cs <> ts -- Crucially left-biased.
                  ext = filter (`S.member` fvVars (freeInType tp')) bound
              modifyConstraints $
                M.insert vn (lvl, Constraint (RetType ext tp') usage)
              unifySharedConstructors onDims usage bound bcs required_cs ts
          | otherwise ->
              unsharedConstructors required_cs ts =<< typeVarNotes vn
        Scalar (TypeVar _ (QualName [] v) []) -> do
          case M.lookup v constraints of
            Just (_, HasConstrs _ v_cs _) ->
              unifySharedConstructors onDims usage bound bcs required_cs v_cs
            Just (_, NoConstraint {}) -> pure ()
            Just (_, Equality {}) -> pure ()
            _ -> do
              notes <- (<>) <$> typeVarNotes vn <*> typeVarNotes v
              noSumType notes
          link
          modifyConstraints $
            M.insertWith
              combineConstrs
              v
              (lvl, HasConstrs l required_cs old_usage)
          where
            combineConstrs (_, HasConstrs l1 cs1 usage1) (_, HasConstrs l2 cs2 _) =
              (lvl, HasConstrs (l1 `min` l2) (M.union cs1 cs2) usage1)
            combineConstrs hasCs _ = hasCs
        _ -> noSumType =<< typeVarNotes vn
    _ -> link
  where
    unsharedConstructors cs1 cs2 notes =
      unifyError
        usage
        notes
        bcs
        (unsharedConstructorsMsg cs1 cs2)
    noSumType notes =
      unifyError
        usage
        notes
        bcs
        "Cannot unify a sum type with a non-sum type."
    noRecordType notes =
      unifyError
        usage
        notes
        bcs
        "Cannot unify a record type with a non-record type."

linkVarToDim ::
  (MonadUnify m) =>
  Usage ->
  BreadCrumbs ->
  VName ->
  Level ->
  Exp ->
  m ()
linkVarToDim usage bcs vn lvl e = do
  constraints <- getConstraints

  mapM_ (checkVar constraints) $ fvVars $ freeInExp e

  modifyConstraints $ M.insert vn (lvl, Size (Just e) usage)
  where
    checkVar _ dim'
      | vn == dim' = do
          notes <- dimNotes usage e
          unifyError usage notes bcs $
            "Occurs check: cannot instantiate"
              <+> dquotes (prettyName vn)
              <+> "with"
              <+> dquotes (pretty e)
              <+> "."
    checkVar constraints dim'
      | Just (dim_lvl, c) <- dim' `M.lookup` constraints,
        dim_lvl >= lvl =
          case c of
            ParamSize {} -> do
              notes <- dimNotes usage e
              unifyError usage notes bcs $
                "Cannot link size"
                  <+> dquotes (prettyName vn)
                  <+> "to"
                  <+> dquotes (pretty e)
                  <+> "(scope violation)."
                  </> "This is because"
                  <+> dquotes (pretty $ qualName dim')
                  <+> "is not in scope when"
                  <+> dquotes (prettyName vn)
                  <+> "is introduced."
            _ -> modifyConstraints $ M.insert dim' (lvl, c)
    checkVar _ _ = pure ()

-- | Assert that this type must be one of the given primitive types.
mustBeOneOf :: (MonadUnify m) => [PrimType] -> Usage -> StructType -> m ()
mustBeOneOf [req_t] usage t = unify usage (Scalar (Prim req_t)) t
mustBeOneOf ts usage t = do
  t' <- normType t
  constraints <- getConstraints
  let isRigid' v = isRigid v constraints

  case t' of
    Scalar (TypeVar _ (QualName [] v) [])
      | not $ isRigid' v -> linkVarToTypes usage v ts
    Scalar (Prim pt) | pt `elem` ts -> pure ()
    _ -> failure
  where
    failure =
      unifyError usage mempty noBreadCrumbs $
        "Cannot unify type"
          <+> dquotes (pretty t)
          <+> "with any of "
          <> commasep (map pretty ts)
          <> "."

linkVarToTypes :: (MonadUnify m) => Usage -> VName -> [PrimType] -> m ()
linkVarToTypes usage vn ts = do
  vn_constraint <- M.lookup vn <$> getConstraints
  case vn_constraint of
    Just (lvl, Overloaded vn_ts vn_usage) ->
      case ts `intersect` vn_ts of
        [] ->
          unifyError usage mempty noBreadCrumbs $
            "Type constrained to one of"
              <+> commasep (map pretty ts)
              <+> "but also one of"
              <+> commasep (map pretty vn_ts)
              <+> "due to"
              <+> pretty vn_usage
              <> "."
        ts' -> modifyConstraints $ M.insert vn (lvl, Overloaded ts' usage)
    Just (_, HasConstrs _ _ vn_usage) ->
      unifyError usage mempty noBreadCrumbs $
        "Type constrained to one of"
          <+> commasep (map pretty ts)
          <> ", but also inferred to be sum type due to"
            <+> pretty vn_usage
          <> "."
    Just (_, HasFields _ _ vn_usage) ->
      unifyError usage mempty noBreadCrumbs $
        "Type constrained to one of"
          <+> commasep (map pretty ts)
          <> ", but also inferred to be record due to"
            <+> pretty vn_usage
          <> "."
    Just (lvl, _) -> modifyConstraints $ M.insert vn (lvl, Overloaded ts usage)
    Nothing ->
      unifyError usage mempty noBreadCrumbs $
        "Cannot constrain type to one of" <+> commasep (map pretty ts)

-- | Assert that this type must support equality.
equalityType ::
  (MonadUnify m, Pretty (Shape dim), Pretty u) =>
  Usage ->
  TypeBase dim u ->
  m ()
equalityType usage t = do
  unless (orderZero t) $
    unifyError usage mempty noBreadCrumbs $
      "Type " <+> dquotes (pretty t) <+> "does not support equality (may contain function)."
  mapM_ mustBeEquality $ typeVars t
  where
    mustBeEquality vn = do
      constraints <- getConstraints
      case M.lookup vn constraints of
        Just (_, Constraint (RetType [] (Scalar (TypeVar _ (QualName [] vn') []))) _) ->
          mustBeEquality vn'
        Just (_, Constraint (RetType _ vn_t) cusage)
          | not $ orderZero vn_t ->
              unifyError usage mempty noBreadCrumbs $
                "Type"
                  <+> dquotes (pretty t)
                  <+> "does not support equality."
                  </> "Constrained to be higher-order due to"
                  <+> pretty cusage
                  <+> "."
          | otherwise -> pure ()
        Just (lvl, NoConstraint _ _) ->
          modifyConstraints $ M.insert vn (lvl, Equality usage)
        Just (_, Overloaded _ _) ->
          pure () -- All primtypes support equality.
        Just (_, Equality {}) ->
          pure ()
        _ ->
          unifyError usage mempty noBreadCrumbs $
            "Type" <+> prettyName vn <+> "does not support equality."

zeroOrderTypeWith ::
  (MonadUnify m) =>
  Usage ->
  BreadCrumbs ->
  StructType ->
  m ()
zeroOrderTypeWith usage bcs t = do
  unless (orderZero t) $
    unifyError usage mempty bcs $
      "Type" </> indent 2 (pretty t) </> "found to be functional."
  mapM_ mustBeZeroOrder . S.toList . typeVars =<< normType t
  where
    mustBeZeroOrder vn = do
      constraints <- getConstraints
      case M.lookup vn constraints of
        Just (lvl, NoConstraint _ _) ->
          modifyConstraints $ M.insert vn (lvl, NoConstraint Unlifted usage)
        Just (lvl, HasFields _ fs _) ->
          modifyConstraints $ M.insert vn (lvl, HasFields Unlifted fs usage)
        Just (lvl, HasConstrs _ cs _) ->
          modifyConstraints $ M.insert vn (lvl, HasConstrs Unlifted cs usage)
        Just (_, ParamType Lifted ploc) ->
          unifyError usage mempty bcs $
            "Type parameter"
              <+> dquotes (prettyName vn)
              <+> "at"
              <+> pretty (locStr ploc)
              <+> "may be a function."
        _ -> pure ()

-- | Assert that this type must be zero-order.
zeroOrderType ::
  (MonadUnify m) => Usage -> T.Text -> StructType -> m ()
zeroOrderType usage desc =
  zeroOrderTypeWith usage $ breadCrumb bc noBreadCrumbs
  where
    bc = Matching $ "When checking" <+> textwrap desc

arrayElemTypeWith ::
  (MonadUnify m, Pretty (Shape dim), Pretty u) =>
  Usage ->
  BreadCrumbs ->
  TypeBase dim u ->
  m ()
arrayElemTypeWith usage bcs t = do
  unless (orderZero t) $
    unifyError usage mempty bcs $
      "Type" </> indent 2 (pretty t) </> "found to be functional."
  mapM_ mustBeZeroOrder . S.toList . typeVars $ t
  where
    mustBeZeroOrder vn = do
      constraints <- getConstraints
      case M.lookup vn constraints of
        Just (lvl, NoConstraint _ _) ->
          modifyConstraints $ M.insert vn (lvl, NoConstraint Unlifted usage)
        Just (_, ParamType l ploc)
          | l `elem` [Lifted, SizeLifted] ->
              unifyError usage mempty bcs $
                "Type parameter"
                  <+> dquotes (prettyName vn)
                  <+> "bound at"
                  <+> pretty (locStr ploc)
                  <+> "is lifted and cannot be an array element."
        _ -> pure ()

-- | Assert that this type must be valid as an array element.
arrayElemType ::
  (MonadUnify m, Pretty (Shape dim), Pretty u) =>
  Usage ->
  T.Text ->
  TypeBase dim u ->
  m ()
arrayElemType usage desc =
  arrayElemTypeWith usage $ breadCrumb bc noBreadCrumbs
  where
    bc = Matching $ "When checking" <+> textwrap desc

unifySharedFields ::
  (MonadUnify m) =>
  UnifySizes m ->
  Usage ->
  [VName] ->
  BreadCrumbs ->
  M.Map Name StructType ->
  M.Map Name StructType ->
  m ()
unifySharedFields onDims usage bound bcs fs1 fs2 =
  forM_ (M.toList $ M.intersectionWith (,) fs1 fs2) $ \(f, (t1, t2)) ->
    unifyWith onDims usage bound (breadCrumb (MatchingFields [f]) bcs) t1 t2

unifySharedConstructors ::
  (MonadUnify m) =>
  UnifySizes m ->
  Usage ->
  [VName] ->
  BreadCrumbs ->
  M.Map Name [StructType] ->
  M.Map Name [StructType] ->
  m ()
unifySharedConstructors onDims usage bound bcs cs1 cs2 =
  forM_ (M.toList $ M.intersectionWith (,) cs1 cs2) $ \(c, (f1, f2)) ->
    unifyConstructor c f1 f2
  where
    unifyConstructor c f1 f2
      | length f1 == length f2 = do
          let bcs' = breadCrumb (MatchingConstructor c) bcs
          zipWithM_ (unifyWith onDims usage bound bcs') f1 f2
      | otherwise =
          unifyError usage mempty bcs $
            "Cannot unify constructor" <+> dquotes (prettyName c) <> "."

-- | In @mustHaveConstr usage c t fs@, the type @t@ must have a
-- constructor named @c@ that takes arguments of types @ts@.
mustHaveConstr ::
  (MonadUnify m) =>
  Usage ->
  Name ->
  StructType ->
  [StructType] ->
  m ()
mustHaveConstr usage c t fs = do
  constraints <- getConstraints
  case t of
    Scalar (TypeVar _ (QualName _ tn) [])
      | Just (lvl, NoConstraint l _) <- M.lookup tn constraints -> do
          mapM_ (scopeCheck usage noBreadCrumbs tn lvl) fs
          modifyConstraints $ M.insert tn (lvl, HasConstrs l (M.singleton c fs) usage)
      | Just (lvl, HasConstrs l cs _) <- M.lookup tn constraints ->
          case M.lookup c cs of
            Nothing ->
              modifyConstraints $
                M.insert tn (lvl, HasConstrs l (M.insert c fs cs) usage)
            Just fs'
              | length fs == length fs' -> zipWithM_ (unify usage) fs fs'
              | otherwise ->
                  unifyError usage mempty noBreadCrumbs $
                    "Different arity for constructor" <+> dquotes (pretty c) <> "."
    Scalar (Sum cs) ->
      case M.lookup c cs of
        Nothing ->
          unifyError usage mempty noBreadCrumbs $
            "Constuctor" <+> dquotes (pretty c) <+> "not present in type."
        Just fs'
          | length fs == length fs' -> zipWithM_ (unify usage) fs fs'
          | otherwise ->
              unifyError usage mempty noBreadCrumbs $
                "Different arity for constructor" <+> dquotes (pretty c) <+> "."
    _ ->
      unify usage t $ Scalar $ Sum $ M.singleton c fs

mustHaveFieldWith ::
  (MonadUnify m) =>
  UnifySizes m ->
  Usage ->
  [VName] ->
  BreadCrumbs ->
  Name ->
  StructType ->
  m StructType
mustHaveFieldWith onDims usage bound bcs l t = do
  constraints <- getConstraints
  l_type <- newTypeVar (srclocOf usage) "t"
  case t of
    Scalar (TypeVar _ (QualName _ tn) [])
      | Just (lvl, NoConstraint {}) <- M.lookup tn constraints -> do
          scopeCheck usage bcs tn lvl l_type
          modifyConstraints $ M.insert tn (lvl, HasFields Lifted (M.singleton l l_type) usage)
          pure l_type
      | Just (lvl, HasFields lifted fields _) <- M.lookup tn constraints -> do
          case M.lookup l fields of
            Just t' -> unifyWith onDims usage bound bcs l_type t'
            Nothing ->
              modifyConstraints $
                M.insert
                  tn
                  (lvl, HasFields lifted (M.insert l l_type fields) usage)
          pure l_type
    Scalar (Record fields)
      | Just t' <- M.lookup l fields -> do
          unify usage l_type t'
          pure t'
      | otherwise ->
          unifyError usage mempty bcs $
            "Attempt to access field"
              <+> dquotes (pretty l)
              <+> " of value of type"
              <+> pretty (toStructural t)
              <> "."
    _ -> do
      unify usage t $ Scalar $ Record $ M.singleton l l_type
      pure l_type

-- | Assert that some type must have a field with this name and type.
mustHaveField ::
  (MonadUnify m) =>
  Usage ->
  Name ->
  StructType ->
  m StructType
mustHaveField usage = mustHaveFieldWith (unifySizes usage) usage mempty noBreadCrumbs

newDimOnMismatch ::
  (MonadUnify m) =>
  SrcLoc ->
  StructType ->
  StructType ->
  m (StructType, [VName])
newDimOnMismatch loc t1 t2 = do
  (t, seen) <- runStateT (matchDims onDims t1 t2) mempty
  pure (t, M.elems seen)
  where
    r = RigidCond t1 t2
    onDims _ d1 d2
      | d1 == d2 = pure d1
      | otherwise = do
          -- Remember mismatches we have seen before and reuse the
          -- same new size.
          maybe_d <- gets $ M.lookup (d1, d2)
          case maybe_d of
            Just d -> pure $ sizeFromName (qualName d) loc
            Nothing -> do
              d <- lift $ newRigidDim loc r "differ"
              modify $ M.insert (d1, d2) d
              pure $ sizeFromName (qualName d) loc

-- | Like unification, but creates new size variables where mismatches
-- occur.  Returns the new dimensions thus created.
unifyMostCommon ::
  (MonadUnify m) =>
  Usage ->
  StructType ->
  StructType ->
  m (StructType, [VName])
unifyMostCommon usage t1 t2 = do
  -- We are ignoring the dimensions here, because any mismatches
  -- should be turned into fresh size variables.
  let allOK _ _ _ _ _ = pure ()
  unifyWith allOK usage mempty noBreadCrumbs t1 t2
  t1' <- normTypeFully t1
  t2' <- normTypeFully t2
  newDimOnMismatch (srclocOf usage) t1' t2'

-- Simple MonadUnify implementation.

type UnifyMState = (Constraints, Int)

newtype UnifyM a = UnifyM (StateT UnifyMState (Except TypeError) a)
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadState UnifyMState,
      MonadError TypeError
    )

newVar :: Name -> UnifyM VName
newVar name = do
  (x, i) <- get
  put (x, i + 1)
  pure $ VName (mkTypeVarName name i) i

instance MonadUnify UnifyM where
  getConstraints = gets fst
  putConstraints x = modify $ \(_, i) -> (x, i)

  newTypeVar loc name = do
    v <- newVar name
    modifyConstraints $ M.insert v (0, NoConstraint Lifted $ Usage Nothing loc)
    pure $ Scalar $ TypeVar mempty (qualName v) []

  newDimVar usage rigidity name = do
    dim <- newVar name
    case rigidity of
      Rigid src ->
        modifyConstraints $
          M.insert dim (0, UnknownSize (srclocOf usage) src)
      Nonrigid ->
        modifyConstraints $
          M.insert dim (0, Size Nothing usage)
    pure dim

  curLevel = pure 1

  unifyError loc notes bcs doc =
    throwError $ TypeError (locOf loc) notes $ doc <> pretty bcs

  matchError loc notes bcs t1 t2 =
    throwError $ TypeError (locOf loc) notes $ doc <> pretty bcs
    where
      doc =
        "Types"
          </> indent 2 (pretty t1)
          </> "and"
          </> indent 2 (pretty t2)
          </> "do not match."

runUnifyM :: [TypeParam] -> [TypeParam] -> UnifyM a -> Either TypeError a
runUnifyM rigid_tparams nonrigid_tparams (UnifyM m) =
  runExcept $ evalStateT m (constraints, 0)
  where
    constraints =
      M.fromList $
        map nonrigid nonrigid_tparams <> map rigid rigid_tparams
    nonrigid (TypeParamDim p loc) = (p, (1, Size Nothing $ Usage Nothing loc))
    nonrigid (TypeParamType l p loc) = (p, (1, NoConstraint l $ Usage Nothing loc))
    rigid (TypeParamDim p loc) = (p, (0, ParamSize loc))
    rigid (TypeParamType l p loc) = (p, (0, ParamType l loc))

-- | Perform a unification of two types outside a monadic context.
-- The first list of type parameters are rigid but may have liftedness
-- constraints; the second list of type parameters are allowed to be
-- instantiated. All other types are considered rigid with no
-- constraints.
doUnification ::
  Loc ->
  [TypeParam] ->
  [TypeParam] ->
  StructType ->
  StructType ->
  Either TypeError StructType
doUnification loc rigid_tparams nonrigid_tparams t1 t2 =
  runUnifyM rigid_tparams nonrigid_tparams $ do
    unify (Usage Nothing (srclocOf loc)) t1 t2
    normTypeFully t2

-- Note [Linking variables to sum types]
--
-- Consider the case when unifying a result type
--
--   i32 -> ?[n].(#foo [n]bool)
--
-- with
--
--   i32 -> ?[k].a
--
-- where 'a' has a HasConstrs constraint saying that it must have at
-- least a constructor of type '#foo [0]bool'.
--
-- This unification should succeed, but we must not merely link 'a' to
-- '#foo [n]bool', as 'n' is not free.  Instead we should instantiate
-- 'a' to be a concrete sum type (because now we know exactly which
-- constructor labels it must have), and unify each of its constructor
-- payloads with the corresponding expected payload.
