{-# LANGUAGE LambdaCase #-}

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
    allDimsFreshInType,
    dimNotes,
    normTypeFully,
    unify,
    unifyMostCommon,
    doUnification,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Bitraversable
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.TypeChecker.Constraints (CtTy (..), Level, Reason (..), TyVarInfo (..))
import Language.Futhark.TypeChecker.Error
import Language.Futhark.TypeChecker.Monad hiding (BoundV)
import Language.Futhark.TypeChecker.TySolve qualified as TySolve
import Language.Futhark.TypeChecker.Types

-- | A usage that caused a type constraint.
data Usage = Usage (Maybe T.Text) Loc
  deriving (Show)

-- | Construct a 'Usage' from a location and a description.
mkUsage :: (Located a) => a -> T.Text -> Usage
mkUsage = flip (Usage . Just) . locOf

-- | Construct a 'Usage' that has just a location, but no particular
-- description.
mkUsage' :: (Located a) => a -> Usage
mkUsage' = Usage Nothing . locOf

instance Pretty Usage where
  pretty (Usage Nothing loc) = "use at " <> textwrap (locText loc)
  pretty (Usage (Just s) loc) = textwrap s <+> "at" <+> textwrap (locText loc)

instance Located Usage where
  locOf (Usage _ loc) = locOf loc

-- | A constraint on a yet-ambiguous size variable, or information
-- about a rigid type parameter or size.
data Constraint
  = ParamSize Loc
  | -- | Is not actually a type, but a term-level size,
    -- possibly already set to something specific.
    Size (Maybe Exp) Usage
  | -- | A size that does not unify with anything -
    -- created from the result of applying a function
    -- whose return size is existential, or otherwise
    -- hiding a size.
    UnknownSize Loc RigidSource
  | -- | A size arising from instantiating a type parameter (of the
    -- given liftedness) with a type whose sizes are not yet known.
    -- In contrast to an ordinary 'Size', unification may determine
    -- that this size is actually existential (unless the type
    -- parameter is unlifted), in which case the constraint is
    -- replaced with 'ExistentialSize'. See Note [Size Inference] in
    -- Language.Futhark.TypeChecker.Terms.
    InstSize Liftedness Usage
  | -- | Another occurrence of the instantiated size denoted by the
    -- given canonical size variable (an 'InstSize'). Kept distinct
    -- from the canonical variable because if the size turns out to
    -- be existential, every occurrence must be a distinct
    -- existential. The integer identifies the occurrence of the
    -- instantiated type parameter that this size is part of; copies
    -- from the same occurrence absorbed from the same source denote
    -- the same existential size.
    CopySize VName Int Usage
  | -- | An instantiated size that unification has determined to
    -- correspond to an existential size, possibly with the variable
    -- it was unified with (used to identify existentials with the
    -- same origin). Each variable constrained by this (or a
    -- 'CopySize' pointing to it) is turned into a rigid size when
    -- the enclosing function application is complete. The second
    -- field, when present, means that a *rigid* unknown size was
    -- absorbed: the existential then stands for a size that is
    -- actually computed at that location, and if it cannot be bound
    -- anywhere, it becomes a rigid unknown size there (see
    -- bindExistentialInsts), subjecting it to the causality check.
    ExistentialSize (Maybe VName) (Maybe Loc) Usage
  deriving (Show)

instance Located Constraint where
  locOf (ParamSize loc) = locOf loc
  locOf (Size _ usage) = locOf usage
  locOf (UnknownSize loc _) = locOf loc
  locOf (InstSize _ usage) = locOf usage
  locOf (CopySize _ _ usage) = locOf usage
  locOf (ExistentialSize _ _ usage) = locOf usage

-- | Mapping from fresh type variables, instantiated from the type
-- schemes of polymorphic functions, to (possibly) specific types as
-- determined on application and the location of that application, or
-- a partial constraint on their type.
type Constraints = M.Map VName (Level, Constraint)

lookupSubst :: VName -> Constraints -> Maybe (Subst StructRetType)
lookupSubst v constraints = case snd <$> M.lookup v constraints of
  Just (Size (Just d) _) ->
    Just $ ExpSubst $ applySubst (`lookupSubst` constraints) d
  Just (CopySize c _ _)
    -- If the canonical size has been resolved to an actual size, we
    -- are equal to that size. Otherwise (canonical size still
    -- pending, or existential) we stand apart under our own name.
    | Just (Size (Just _) _) <- snd <$> M.lookup c constraints ->
        lookupSubst c constraints
  _ -> Nothing

-- | The source of a rigid size.
data RigidSource
  = -- | A function argument that is not a constant or variable name.
    RigidArg (Maybe (QualName VName)) T.Text
  | -- | An existential return size.
    RigidRet (Maybe (QualName VName))
  | -- | Similarly to 'RigidRet', but produce by a loop.
    RigidLoop
  | -- | Produced by a complicated slice expression.
    RigidSlice (Maybe Size) T.Text
  | -- | Produced by a complicated range expression.
    RigidRange
  | -- | Mismatch in branches.
    RigidCond StructType StructType
  | -- | Invented during unification.
    RigidUnify
  | -- | A name used in a size went out of scope.
    RigidOutOfScope Loc VName
  deriving (Eq, Ord, Show)

-- | The ridigity of a size variable.  All rigid sizes are tagged with
-- information about how they were generated.
data Rigidity = Rigid RigidSource | Nonrigid
  deriving (Eq, Ord, Show)

prettySource :: Loc -> Loc -> RigidSource -> Doc ()
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
prettySource ctx loc (RigidOutOfScope boundloc v) =
  "is an unknown size arising from "
    <> dquotes (prettyName v)
    <> " going out of scope at "
    <> pretty (locStrRel ctx loc)
    <> "."
      </> "Originally bound at "
    <> pretty (locStrRel ctx boundloc)
    <> "."
prettySource _ _ RigidUnify =
  textwrap "is an artificial size invented during unification of functions with anonymous sizes."
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
        dquotes (pretty d) <+> prettySource (locOf ctx) loc rsrc
    _ -> pure mempty
dimNotes _ _ = pure mempty

-- | Monads that which to perform unification must implement this type
-- class.
class (Monad m) => MonadUnify m where
  getConstraints :: m Constraints
  putConstraints :: Constraints -> m ()
  modifyConstraints :: (Constraints -> Constraints) -> m ()
  modifyConstraints f = do
    x <- getConstraints
    putConstraints $ f x

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

rigidConstraint :: Constraint -> Bool
rigidConstraint ParamSize {} = True
rigidConstraint UnknownSize {} = True
rigidConstraint ExistentialSize {} = True
rigidConstraint _ = False

-- | If the given type variable is nonrigid, what is its level?
isNonRigid :: VName -> Constraints -> Maybe Level
isNonRigid v constraints = do
  (lvl, c) <- M.lookup v constraints
  case c of
    -- A copy is as rigid as its canonical size.
    CopySize c' _ _ | Just (_, c'') <- M.lookup c' constraints -> do
      guard $ not $ rigidConstraint c''
      pure lvl
    _ -> do
      guard $ not $ rigidConstraint c
      pure lvl

type UnifySizes m =
  BreadCrumbs -> [VName] -> (VName -> Maybe Int) -> Exp -> Exp -> m ()

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

    subunify ord bound bcs t1' t2' = do
      constraints <- getConstraints

      let nonrigid v = isNonRigid v constraints

          failure = matchError (srclocOf usage) mempty bcs t1' t2'

          unifyTypeArg bcs' (TypeArgDim d1) (TypeArgDim d2) =
            onDims' bcs' (swap ord d1 d2)
          unifyTypeArg bcs' (TypeArgType t) (TypeArgType arg_t) =
            subunify ord bound bcs' t arg_t
          unifyTypeArg _ _ _ = failure

          onDims' bcs' (d1, d2) =
            onDims
              bcs'
              bound
              nonrigid
              (applySubst (`lookupSubst` constraints) d1)
              (applySubst (`lookupSubst` constraints) d2)

      -- The types are structurally identical, as this has already
      -- been verified by the unsized type checker - we are here only
      -- to unify their sizes (and check consumption and uniqueness
      -- for functions). The 'failure' cases can be reached when the
      -- types contain distinct abstract types that the unsized
      -- checking could not distinguish, and serve as a backstop for
      -- anything it may have missed.
      case (t1', t2') of
        (Scalar (Prim pt1), Scalar (Prim pt2))
          | pt1 == pt2 -> pure ()
        ( Scalar (Record fs),
          Scalar (Record arg_fs)
          )
            | M.keys fs == M.keys arg_fs ->
                forM_ (M.toList $ M.intersectionWith (,) fs arg_fs) $ \(f, (t1, t2)) ->
                  subunify ord bound (matchingField f <> bcs) t1 t2
        ( Scalar (Sum cs),
          Scalar (Sum arg_cs)
          )
            | M.keys cs == M.keys arg_cs,
              fmap length cs == fmap length arg_cs ->
                forM_ (M.toList $ M.intersectionWith (,) cs arg_cs) $ \(c, (ts1, ts2)) ->
                  zipWithM_ (subunify ord bound (matchingConstructor c <> bcs)) ts1 ts2
        ( Scalar (TypeVar _ (QualName _ tn) targs),
          Scalar (TypeVar _ (QualName _ arg_tn) arg_targs)
          )
            | tn == arg_tn,
              length targs == length arg_targs -> do
                let bcs' = matching "When matching type arguments." <> bcs
                zipWithM_ (unifyTypeArg bcs') targs arg_targs
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
                unifyError usage mempty bcs $
                  "Return types"
                    </> indent 2 (pretty b1)
                    </> "and"
                    </> indent 2 (pretty b2)
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
                  (matching "When matching parameter types." <> bcs)
                  a1
                  a2
                subunify
                  ord
                  bound'
                  (matching "When matching return types." <> bcs)
                  (toStruct b1')
                  (toStruct b2')

                -- If a flexible existential size was resolved to a pending
                -- instantiated size, then that size is existential. This is how
                -- a hole absorbs an existential size from a type it is unified
                -- with. See Note [Size Inference] in
                -- Language.Futhark.TypeChecker.Terms.
                constraints_after <- getConstraints
                -- An existential that was already registered as a
                -- rigid unknown size before we made it unifiable
                -- above (e.g. a pending instantiated size bound by
                -- checkApply) stands for a size that is actually
                -- computed somewhere, so absorbing it incurs a
                -- causality obligation.
                let rigidPre d = case snd <$> M.lookup d constraints of
                      Just (UnknownSize dloc _)
                        | dloc == mempty -> Just $ locOf usage
                        | otherwise -> Just dloc
                      _ -> Nothing
                    existentialise d v usage' =
                      modifyConstraints $
                        M.adjust (fmap $ const $ ExistentialSize (Just d) (rigidPre d) usage') v
                    absorbExt d
                      | Just (Size (Just de) _) <- snd <$> M.lookup d constraints_after,
                        Var de_v _ _ <- applySubst (`lookupSubst` constraints_after) de =
                          case snd <$> M.lookup (qualLeaf de_v) constraints_after of
                            Just (InstSize l usage')
                              | l /= Unlifted ->
                                  existentialise d (qualLeaf de_v) usage'
                            Just (CopySize c _ usage')
                              | Just (InstSize l _) <- snd <$> M.lookup c constraints_after,
                                l /= Unlifted ->
                                  existentialise d c usage'
                            _ -> pure ()
                      | otherwise = pure ()
                mapM_ absorbExt (b1_dims <> b2_dims)

                -- Delete the size variables we introduced to represent the
                -- existential sizes.
                modifyConstraints $ \m -> L.foldl' (flip M.delete) m (b1_dims <> b2_dims)
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
        ( Array _ (Shape (t1_d : t1_ds)) t1_et,
          Array _ (Shape (t2_d : t2_ds)) t2_et
          ) -> do
            onDims' bcs (swap ord t1_d t2_d)
            subunify
              ord
              bound
              bcs
              (arrayOf (Shape t1_ds) (Scalar t1_et))
              (arrayOf (Shape t2_ds) (Scalar t2_et))
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
unifySizes usage bcs bound _ e1 e2 = do
  -- A size arising from a type parameter instantiation may be linked
  -- to sizes bound within the instantiated type itself (reconstructing
  -- a dependent function type), and when it meets any other bound
  -- size (an existential), it is determined to be existential itself,
  -- rather than this being an error. This is the only way we can know
  -- how instantiated sizes depend on binders and existentials. See
  -- Note [Size Inference] in Language.Futhark.TypeChecker.Terms.
  linked <- (||) <$> maybeLocalLink e1 e2 <*> maybeLocalLink e2 e1
  absorbed <-
    if linked
      then pure True
      else (||) <$> maybeAbsorb e1 e2 <*> maybeAbsorb e2 e1
  unless absorbed $ do
    notes <- (<>) <$> dimNotes usage e1 <*> dimNotes usage e2
    anon1 <- instMeetsAnonymous e1 e2
    anon2 <- instMeetsAnonymous e2 e1
    if anon1 || anon2
      then
        unifyError usage notes bcs $
          "Sizes"
            <+> dquotes (pretty e1)
            <+> "and"
            <+> dquotes (pretty e2)
            <+> "do not match."
            </> textwrap "This is because a type parameter would be instantiated with a type containing anonymous sizes."
      else
        unifyError usage notes bcs $
          "Sizes"
            <+> dquotes (pretty e1)
            <+> "and"
            <+> dquotes (pretty e2)
            <+> "do not match."
  where
    instConstraint constraints v = do
      c <- snd <$> M.lookup v constraints
      case c of
        InstSize {} -> Just c
        CopySize {} -> Just c
        ExistentialSize {} -> Just c
        _ -> Nothing
    -- If the absorbed size is a rigid unknown size, then the
    -- existential stands for a size that is actually computed
    -- somewhere, and uses of it are subject to the causality check.
    -- Sizes bound in the type itself (existentials of a declared
    -- type, parameters) carry no such obligation.
    existentialise v other usage' = do
      constraints <- getConstraints
      let rigidLoc w = case snd <$> M.lookup w constraints of
            Just (UnknownSize wloc _)
              | wloc == mempty -> Just $ locOf usage
              | otherwise -> Just wloc
            _ -> Nothing
          computed_at =
            listToMaybe $ mapMaybe rigidLoc $ S.toList $ fvVars $ freeInExp other
      modifyConstraints $ M.adjust (fmap $ const $ ExistentialSize key computed_at usage') v
      where
        key = case other of
          Var other_v _ _ -> Just $ qualLeaf other_v
          _ -> Nothing
    -- Linking is fine if every bound size mentioned is a binder of
    -- the instantiated type itself (a registered 'ParamSize'), as
    -- instantiated size variables occur exactly once, and binders
    -- are cloned between occurrences of the instantiated type.
    maybeLocalLink (Var v _ _) other
      | anyBound bound other,
        qualLeaf v `notElem` bound = do
          constraints <- getConstraints
          let mentioned = filter (`elem` bound) $ S.toList $ fvVars $ freeInExp other
              registeredBinder bv = case snd <$> M.lookup bv constraints of
                Just (ParamSize _) -> True
                _ -> False
          case instConstraint constraints (qualLeaf v) of
            Just c
              | all registeredBinder mentioned,
                notExistential c -> do
                  modifyConstraints $
                    M.adjust (fmap $ const $ Size (Just other) usage) (qualLeaf v)
                  pure True
            _ -> pure False
      where
        notExistential ExistentialSize {} = False
        notExistential _ = True
    maybeLocalLink _ _ = pure False
    maybeAbsorb (Var v _ _) other
      | anyBound bound other,
        qualLeaf v `notElem` bound = do
          constraints <- getConstraints
          case snd <$> M.lookup (qualLeaf v) constraints of
            Just (InstSize l usage')
              | l /= Unlifted ->
                  True <$ existentialise (qualLeaf v) other usage'
            Just ExistentialSize {} ->
              pure True
            Just (CopySize c _ usage') ->
              case snd <$> M.lookup c constraints of
                Just (InstSize l _)
                  | l /= Unlifted -> True <$ existentialise c other usage'
                Just ExistentialSize {} -> pure True
                _ -> pure False
            _ -> pure False
    maybeAbsorb _ _ = pure False
    instMeetsAnonymous (Var v _ _) other
      | anyBound bound other = do
          constraints <- getConstraints
          pure $ isJust $ instConstraint constraints $ qualLeaf v
    instMeetsAnonymous _ _ = pure False

-- | Unifies two types.
unify :: (MonadUnify m) => Usage -> StructType -> StructType -> m ()
unify usage = unifyWith (unifySizes usage) usage mempty mempty

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

  -- A copy of an instantiated size is equal to its canonical
  -- variable as long as the size is not existential, so links are
  -- expressed in terms of canonical variables: both when the linked
  -- variable is a copy, and when copies occur in the expression
  -- linked to.
  let canonize v = case snd <$> M.lookup v constraints of
        Just (CopySize c _ _) ->
          Just $ ExpSubst $ sizeFromName (qualName c) $ srclocOf usage
        _ -> Nothing
      e' = applySubst canonize e

  case snd <$> M.lookup vn constraints of
    Just (CopySize c _ _)
      | Just (c_lvl, _) <- M.lookup c constraints ->
          linkVarToDim usage bcs c c_lvl e'
    _
      -- Linking a size to itself is a no-op. This can occur when
      -- unifying a canonical size with one of its own copies.
      | Var (QualName _ e_v) _ _ <- e',
        e_v == vn ->
          pure ()
      | otherwise -> do
          mapM_ (checkVar constraints) $ fvVars $ freeInExp e'

          modifyConstraints $ M.insert vn (lvl, Size (Just e') usage)
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
                withIndexLink "scope-violation" $
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

newDimOnMismatch ::
  (MonadUnify m) =>
  Loc ->
  StructType ->
  StructType ->
  m (StructType, [VName])
newDimOnMismatch loc t1 t2 = do
  (t, seen) <- runStateT (matchDims onDims t1 t2) mempty
  pure (t, M.elems seen)
  where
    r = RigidCond t1 t2
    same (e1, e2) =
      maybe False (all same) $ similarExps e1 e2
    onDims _ d1 d2
      | same (d1, d2) = pure d1
      | otherwise = do
          -- Remember mismatches we have seen before and reuse the
          -- same new size.
          maybe_d <- gets $ M.lookup (d1, d2)
          case maybe_d of
            Just d -> pure $ sizeFromName (qualName d) $ srclocOf loc
            Nothing -> do
              d <- lift $ newRigidDim loc r "differ"
              modify $ M.insert (d1, d2) d
              pure $ sizeFromName (qualName d) $ srclocOf loc

-- | Like unification, but creates new size variables where mismatches
-- occur.  Returns the new dimensions thus created.
unifyMostCommon ::
  (MonadUnify m) =>
  Usage ->
  StructType ->
  StructType ->
  m (StructType, [VName])
unifyMostCommon usage t1 t2 = do
  -- Like 'unifySizes', except we do not fail on mismatches - these
  -- are instead turned into fresh existential sizes in
  -- 'newDimOnMismatch'. The most annoying thing is that we have to
  -- replicate scope checking, because we don't want to link if it
  -- would fail.
  constraints <- getConstraints

  let expFreeVars = fvVars . freeInExp
      varLevel v = fst <$> M.lookup v constraints

      -- Check that linking to this expression would not fail in linkVarToDim
      -- due to a ParamSize at a level >= the target level. This replicates the
      -- scope check performed by linkVarToDim's checkVar.
      wouldFail lvl v =
        case M.lookup v constraints of
          Just (dim_lvl, ParamSize {}) -> dim_lvl >= lvl
          _ -> False

      -- Can we link a variable at the given level to an expression with the
      -- given free variables? FIXME: something her is fishy. Why do we need to
      -- treat ParamSize specially in wouldFail? Why is the level check for the
      -- other variables not enough?
      canLink lvl vn bound fvs =
        L.foldl' max 0 (mapMaybe varLevel $ S.toList fvs) <= lvl
          && not (any (`S.member` fvs) bound)
          && not (any (wouldFail lvl) $ S.toList fvs)
          && not (vn `S.member` fvs)

      onDims bcs bound nonrigid e1 e2
        | Just es <- similarExps e1 e2 =
            mapM_ (uncurry $ onDims bcs bound nonrigid) es
      onDims bcs bound nonrigid (Var v1 _ _) e2
        | Just lvl1 <- nonrigid (qualLeaf v1),
          canLink lvl1 (qualLeaf v1) bound (expFreeVars e2) =
            linkVarToDim usage bcs (qualLeaf v1) lvl1 e2
      onDims bcs bound nonrigid e1 (Var v2 _ _)
        | Just lvl2 <- nonrigid (qualLeaf v2),
          canLink lvl2 (qualLeaf v2) bound (expFreeVars e1) =
            linkVarToDim usage bcs (qualLeaf v2) lvl2 e1
      onDims _ _ _ _ _ = pure ()

  unifyWith onDims usage mempty mempty t1 t2
  t1' <- normTypeFully t1
  t2' <- normTypeFully t2
  newDimOnMismatch (locOf usage) t1' t2'

-- | Replace *all* dimensions with distinct fresh size variables.
allDimsFreshInType ::
  (MonadUnify m) =>
  Usage ->
  Rigidity ->
  Name ->
  TypeBase d als ->
  m (TypeBase Size als, M.Map VName d)
allDimsFreshInType usage r desc t =
  runStateT (bitraverse onDim pure t) mempty
  where
    onDim d = do
      v <- lift $ newDimVar usage r desc
      modify $ M.insert v d
      pure $ sizeFromName (qualName v) $ srclocOf usage

-- Simple pure MonadUnify implementation for unification outside of
-- the term checker. The constraints contain only sizes.

type UnifyMState = (Constraints, Int)

newtype UnifyM a = UnifyM (StateT UnifyMState (Except TypeError) a)
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadState UnifyMState,
      MonadError TypeError
    )

instance MonadUnify UnifyM where
  getConstraints = gets fst
  putConstraints x = modify $ \(_, i) -> (x, i)

  newDimVar usage rigidity name = do
    (x, i) <- get
    put (x, i + 1)
    -- Note that the level is 1, so that fresh sizes may be linked to
    -- the rigid parameters, which are at level 0.
    let dim = VName (mkTypeVarName name i) i
    case rigidity of
      Rigid src ->
        modifyConstraints $
          M.insert dim (1, UnknownSize (locOf usage) src)
      Nonrigid ->
        modifyConstraints $
          M.insert dim (1, Size Nothing usage)
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
        mapMaybe nonrigid nonrigid_tparams <> mapMaybe rigid rigid_tparams
    nonrigid (TypeParamDim p ploc) =
      Just (p, (1, Size Nothing $ Usage Nothing $ locOf ploc))
    nonrigid TypeParamType {} = Nothing
    rigid (TypeParamDim p ploc) = Just (p, (0, ParamSize $ locOf ploc))
    rigid TypeParamType {} = Nothing

-- | Check that two types match, instantiating the nonrigid type
-- parameters of the second type as necessary. This is used when
-- matching a value or type in a module against a specification.
--
-- This works in two phases. First the types are checked while
-- disregarding sizes entirely, using the same constraint solver as
-- the unsized type checker. This also determines the instantiation
-- of the nonrigid type parameters, up to sizes. Then the
-- instantiations, given fresh size variables, are substituted into
-- the second type, and the sizes are checked with ordinary
-- (size-only) unification.
doUnification ::
  Loc ->
  [TypeParam] ->
  [TypeParam] ->
  StructType ->
  StructType ->
  Either TypeError ()
doUnification loc rigid_tparams nonrigid_tparams spec_t t = do
  -- Phase 1: types.
  let typarams =
        M.fromList
          [ (v, (0, l, locOf tploc))
          | TypeParamType l v tploc <- rigid_tparams
          ]
      tyvars =
        M.fromList
          [ (v, (1, TyVarFree (locOf tploc) l))
          | TypeParamType l v tploc <- nonrigid_tparams
          ]
      ct = CtEq (Reason loc) (unsized spec_t) (unsized t)
  (_, solution) <- TySolve.solve [ct] typarams tyvars

  -- The solver does not verify that instantiations respect the
  -- liftedness of the instantiated type parameter, so we check that
  -- here.
  mapM_ (checkLiftedness solution) nonrigid_tparams

  -- Phase 2: sizes.
  runUnifyM rigid_tparams nonrigid_tparams $ do
    -- Give the instantiations of the type parameters fresh size
    -- variables. Crucially, each type parameter is instantiated only
    -- once, so multiple occurrences of the same type parameter will
    -- have the same sizes.
    substs <- fmap (M.fromList . catMaybes) . forM nonrigid_tparams $ \case
      TypeParamType _ v _
        | Just (Right sol_t) <- M.lookup v solution -> do
            (sol_t', _) <-
              allDimsFreshInType (Usage Nothing loc) Nonrigid "d" sol_t
            pure $ Just (v, Subst [] $ RetType [] sol_t')
      _ -> pure Nothing
    unify (Usage Nothing loc) spec_t $ applySubst (`M.lookup` substs) t
  where
    unsized = first $ const ()

    rigid_liftedness =
      M.fromList [(v, l) | TypeParamType l v _ <- rigid_tparams]

    checkLiftedness _ (TypeParamDim {}) = pure ()
    checkLiftedness _ (TypeParamType Lifted _ _) = pure ()
    checkLiftedness solution (TypeParamType l v _)
      | Just (Right inst_t) <- M.lookup v solution = do
          unless (orderZero inst_t) . Left . TypeError loc mempty $
            "Cannot instantiate type parameter"
              <+> dquotes (prettyName v)
              <+> "with functional type"
              </> indent 2 (pretty inst_t)
          case mapMaybe badParam $ S.toList $ typeVars inst_t of
            v' : _ ->
              Left . TypeError loc mempty $
                "Cannot instantiate type parameter"
                  <+> dquotes (prettyName v)
                  <+> "with type containing lifted type parameter"
                  <+> dquotes (prettyName v')
                  <> "."
            [] -> pure ()
      | otherwise = pure ()
      where
        badParam v' = do
          l' <- M.lookup v' rigid_liftedness
          guard $ case l of
            Unlifted -> l' /= Unlifted
            _ -> l' == Lifted
          Just v'
