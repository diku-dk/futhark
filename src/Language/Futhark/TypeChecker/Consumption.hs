-- | Check that a value definition does not violate any consumption constraints,
-- via alias analysis.
--
-- This is a very subtle part of the language. The basic idea is "the result of
-- an expression aliases everything that goes into it", but this is in practice
-- too restrictive when we have functions (and polymorphic and higher-order
-- functions!) as well. However, we also do not want to make the user-facing
-- type system too complicated.
--
-- Our solution is to infer more precise function types, that directly talk
-- about alias information, via parametricity. These never escape into the
-- user-facing language.
--
-- Our overall philosophy is that soundness is (of course) non-negotiable, but
-- simplicity is better than flexibility. Futhark is not a language for very
-- fine-grained reasoning about aliasing.
--
-- As an example of the compromise, @x |> copy@ works and produces a fresh
-- result, whilst @id >-> copy@ loses freshness information.
module Language.Futhark.TypeChecker.Consumption
  ( checkValDef,

    -- * For testing
    Alias (..),
    Aliases,
    TypeAliases,
    inferReturnFreshness,
  )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifoldable
import Data.Bifunctor
import Data.DList qualified as DL
import Data.Foldable
import Data.Functor.Const
import Data.Functor.Identity
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Futhark.Util (nubOrd)
import Futhark.Util.Pretty hiding (space)
import Language.Futhark
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Monad (Notes, TypeError (..), withIndexLink)
import Prelude hiding (mod)

-- | A position within a compound type. A path step is a record field name, or a
-- constructor name followed by the tuple field name of a position in its
-- payload.
type Path = [Name]

-- | A variable together with a path: the component of that variable at that
-- path, as defined by 'traverseLeaves'.  See Note [Locations].
type Location = (VName, Path)

-- | Something a value may share memory with. Every constructor but 'AliasSelf'
-- denotes a 'Location'. Its variable may be in scope, or be free: either it has
-- gone out of scope, or it is an internal name standing for an intermediate
-- value. A free alias behaves more like an equivalence class. See
-- uniqueness-error18.fut for an example of why this is necessary.
data Alias
  = AliasBound Location
  | AliasFree Location
  | -- | Used to represent unknowable internal aliasing, which for example may
    -- occur for a function that returns a nonfresh abstract type. (That
    -- abstract type may internally be a pair of arrays that alias each other.)
    AliasSelf
  deriving (Eq, Ord, Show)

instance Pretty Alias where
  pretty (AliasBound (v, fs)) = prettyAlias v fs
  pretty (AliasFree (v, fs)) = "~" <> prettyAlias v fs
  pretty AliasSelf = "self"

-- | The location an alias refers to.  'AliasSelf' refers to none.
aliasLoc :: Alias -> Maybe Location
aliasLoc (AliasBound l) = Just l
aliasLoc (AliasFree l) = Just l
aliasLoc AliasSelf = Nothing

-- | The variable an alias refers to.  'AliasSelf' does not refer to any
-- variable, as it denotes aliasing internal to a value.
aliasVar :: Alias -> Maybe VName
aliasVar = fmap fst . aliasLoc

-- | Apply a function to every leaf of a type (every component that is neither a
-- record nor a sum) along with its path.
traverseLeaves ::
  (Applicative f) =>
  (Path -> TypeBase dim o -> f (TypeBase dim v)) ->
  TypeBase dim o ->
  f (TypeBase dim v)
traverseLeaves f = go []
  where
    go p (Scalar (Record ts)) =
      Scalar . Record <$> M.traverseWithKey (\k -> go (p ++ [k])) ts
    go p (Scalar (Sum cs)) =
      Scalar . Sum <$> M.traverseWithKey (\c -> zipWithM (\i -> go (p ++ [c, i])) tupleFieldNames) cs
    go p t = f p t

-- | Apply a function to every leaf of a type along with its path.
mapLeaves :: (Path -> TypeBase dim o -> TypeBase dim v) -> TypeBase dim o -> TypeBase dim v
mapLeaves f = runIdentity . traverseLeaves (\p -> Identity . f p)

-- | The leaves of a type along with their paths.
leaves :: TypeBase dim o -> [(Path, TypeBase dim o)]
leaves = getConst . traverseLeaves (\p t -> Const [(p, t)])

-- | The locations these aliases refer to.
aliasLocs :: Aliases -> [Location]
aliasLocs = mapMaybe aliasLoc . S.toList

-- | The variables these aliases refer to.  'AliasSelf' contributes nothing,
-- as it refers to no variable.
aliasVars :: Aliases -> S.Set VName
aliasVars = S.fromList . mapMaybe aliasVar . S.toList

-- | Does this value have internal aliasing, meaning it can neither be consumed
-- nor given a fresh type?  See 'AliasSelf'.
selfAliased :: Aliases -> Bool
selfAliased = S.member AliasSelf

-- | Might two values with these aliases share memory? This is not the same
-- question as whether the sets intersect: 'AliasSelf' denotes a property of a
-- single value rather than a shared referent ('aliasVar' is 'Nothing' for it),
-- so two values that both have internal aliasing are not thereby aliases of
-- each other. Avoid comparing alias sets in any other way than through this
-- function.
overlaps :: Aliases -> Aliases -> Bool
overlaps x y = not $ S.disjoint (referents x) (referents y)
  where
    referents = S.filter (isJust . aliasVar)

prettyAlias :: VName -> [Name] -> Doc ann
prettyAlias v fs = prettyName v <> mconcat (map (("." <>) . prettyName) fs)

instance Pretty (S.Set Alias) where
  pretty = braces . commasep . map pretty . S.toList

-- | Does this alias refer to an in-scope variable?  'AliasFree' has left
-- scope and 'AliasSelf' is no variable at all.  This is not the way to ask
-- whether two values may share memory; see 'overlaps'.
isBoundAlias :: Alias -> Bool
isBoundAlias AliasBound {} = True
isBoundAlias _ = False

-- | What a value may share memory with.
type Aliases = S.Set Alias

type TypeAliases = TypeBase Size Aliases

-- | @t \`setAliases\` als@ returns @t@, but with @als@ substituted for
-- any already present aliases.
setAliases :: TypeBase dim o1 -> o2 -> TypeBase dim o2
setAliases t = addAliases t . const

-- | @t \`addAliases\` f@ returns @t@, but with any already present
-- aliases replaced by @f@ applied to that aliases.
addAliases :: TypeBase dim o1 -> (o1 -> o2) -> TypeBase dim o2
addAliases = flip second

aliases :: TypeAliases -> Aliases
aliases = bifoldMap (const mempty) id

-- | Add an alias to a leaf of a type. That means we assume the type passed in
-- here is not a compound type.
aliasLeaf :: Alias -> TypeAliases -> TypeAliases
aliasLeaf a (Array als shape et) = Array (S.insert a als) shape et
aliasLeaf a (Scalar (TypeVar als tn args)) = Scalar $ TypeVar (S.insert a als) tn args
aliasLeaf a (Scalar (Arrow als mn d ps rt)) = Scalar $ Arrow (S.insert a als) mn d ps rt
aliasLeaf _ t = t

-- | @insertSelfAliases v t@ adds an alias of @v@ to every component of @t@,
-- noting the path at which the component sits.
insertSelfAliases :: VName -> TypeAliases -> TypeAliases
insertSelfAliases v = mapLeaves $ aliasLeaf . AliasBound . (v,)

-- | The aliases of a use of the global @v@, given its declared type and type
-- parameters and the type it is used at. A use of a global name aliases that
-- name, except where parametricity rules it out. In particular, a non-function
-- component whose declared type contains one of the type parameters cannot be
-- (part of) a global, and a function can only yield a value aliasing a global
-- if some nonfresh component of its result is not of that kind. See Note
-- [Parametric results].
globalAliases :: VName -> [TypeParam] -> StructType -> StructType -> TypeAliases
globalAliases v tparams decl = mapLeaves onLeaf . second (const mempty)
  where
    tparams' = S.fromList [p | TypeParamType _ p _ <- tparams]

    -- A type parameter counts only where it is itself a type, not where it is
    -- an argument to an abstract type, which may ignore it.
    parametric :: TypeBase Size u -> Bool
    parametric (Array _ _ et) = parametric $ Scalar et
    parametric (Scalar (TypeVar _ tn _)) = qualLeaf tn `S.member` tparams'
    parametric (Scalar (Record fs)) = any parametric fs
    parametric (Scalar (Sum cs)) = any (any parametric) cs
    parametric _ = False

    decl_leaves = leaves decl

    onLeaf p t =
      case listToMaybe [d | (dp, d) <- decl_leaves, dp `L.isPrefixOf` p] of
        Just d@(Scalar Arrow {})
          | anyResultComponent maybeGlobal (toRes Nonfresh d) ->
              aliasLeaf (AliasBound (v, p)) t
          | otherwise -> t
        Just d | parametric d -> t
        _ -> aliasLeaf (AliasBound (v, p)) t

    maybeGlobal t@(Array Nonfresh _ _) = not $ parametric t
    maybeGlobal t@(Scalar (TypeVar Nonfresh _ _)) = not $ parametric t
    maybeGlobal _ = False

updateAliases :: TypeAliases -> [UpdateStep Info VName] -> TypeAliases -> TypeAliases
updateAliases _ [] ve_als =
  ve_als
updateAliases src_als (UpdateStepField f : rest) ve_als =
  case src_als of
    Scalar (Record fs)
      | Just sub <- M.lookup f fs ->
          Scalar $ Record $ M.insert f (updateAliases sub rest ve_als) fs
    _ ->
      src_als
updateAliases src_als (UpdateStepSlice _ : _) _ = second (const mempty) src_als

data Entry a
  = Consumable {entryAliases :: a}
  | Nonconsumable {entryAliases :: a}
  deriving (Eq, Ord, Show)

instance Functor Entry where
  fmap f (Consumable als) = Consumable $ f als
  fmap f (Nonconsumable als) = Nonconsumable $ f als

data CheckEnv = CheckEnv
  { envVtable :: M.Map VName (Entry TypeAliases),
    -- | Location of the definition we are checking.
    envLoc :: Loc,
    -- | The declared type of a global, along with the type parameters it is
    -- polymorphic in.  This is what lets us exploit parametricity; see Note
    -- [Parametric results].
    envGlobal :: QualName VName -> Maybe ([TypeParam], StructType)
  }

-- | A description of where an artificial compiler-generated
-- intermediate name came from.
data NameReason
  = -- | Name is the result of a function application.
    NameAppRes (Maybe (QualName VName)) SrcLoc
  | NameLoopRes SrcLoc
  | -- | Name ties together the components of the result of the given kind
    -- of branching expression; see Note [Locations].
    NameBranchRes T.Text SrcLoc

nameReason :: SrcLoc -> NameReason -> Doc a
nameReason loc (NameAppRes Nothing apploc) =
  "result of application at" <+> pretty (locStrRel loc apploc)
nameReason loc (NameAppRes fname apploc) =
  "result of applying"
    <+> dquotes (pretty fname)
    <+> parens ("at" <+> pretty (locStrRel loc apploc))
nameReason loc (NameLoopRes apploc) =
  "result of loop at" <+> pretty (locStrRel loc apploc)
nameReason loc (NameBranchRes what eloc) =
  "result of" <+> pretty what <+> "at" <+> pretty (locStrRel loc eloc)

-- | The locations consumed so far, each with where it was consumed.
type Consumed = M.Map Location Loc

-- | Is this location dead because of something in the consumed set?  That is
-- the case if a location on the same variable has been consumed whose path is
-- a prefix of this one, or of which this one is a prefix.  The result is where
-- the killing consumption happened.
deadIn :: Consumed -> Location -> Maybe Loc
deadIn cons (v, p) = listToMaybe $ mapMaybe killing $ M.toList on_v
  where
    on_v = M.takeWhileAntitone ((== v) . fst) $ M.dropWhileAntitone ((< v) . fst) cons
    killing ((_, q), loc)
      | q `L.isPrefixOf` p || p `L.isPrefixOf` q = Just loc
      | otherwise = Nothing

data CheckState = CheckState
  { stateConsumed :: Consumed,
    stateErrors :: DL.DList TypeError,
    stateNames :: M.Map VName NameReason,
    stateCounter :: Int
  }

newtype CheckM a = CheckM (ReaderT CheckEnv (State CheckState) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader CheckEnv,
      MonadState CheckState
    )

runCheckM ::
  (QualName VName -> Maybe ([TypeParam], StructType)) ->
  Loc ->
  CheckM a ->
  (a, [TypeError])
runCheckM globals loc (CheckM m) =
  let (a, s) = runState (runReaderT m env) initial_state
   in (a, DL.toList (stateErrors s))
  where
    env =
      CheckEnv
        { envVtable = mempty,
          envLoc = loc,
          envGlobal = globals
        }
    initial_state =
      CheckState
        { stateConsumed = mempty,
          stateErrors = mempty,
          stateNames = mempty,
          stateCounter = 0
        }

describeVar :: VName -> CheckM (Doc a)
describeVar v = describeLoc (v, [])

-- | Like 'describeVar', but naming a variable written by the programmer
-- without calling it one.
describeName :: VName -> CheckM (Doc a)
describeName v = do
  loc <- asks envLoc
  gets $
    maybe (dquotes (prettyName v)) (nameReason (srclocOf loc))
      . M.lookup v
      . stateNames

-- | Describe a location for the user.  A path into a sum payload is not
-- something the user can write, so the path is cut off at the first sum.
describeLoc :: Location -> CheckM (Doc a)
describeLoc (v, fs) = do
  loc <- asks envLoc
  fs' <- asks $ maybe fs (recordPath fs . entryAliases) . M.lookup v . envVtable
  gets $
    maybe ("variable" <+> dquotes (prettyAlias v fs')) (nameReason (srclocOf loc))
      . M.lookup v
      . stateNames

-- | The part of a path that steps only into records.
recordPath :: Path -> TypeBase dim o -> Path
recordPath (f : fs) (Scalar (Record ts))
  | Just t <- M.lookup f ts = f : recordPath fs t
recordPath _ _ = []

noConsumable :: CheckM a -> CheckM a
noConsumable = local $ \env -> env {envVtable = M.map f $ envVtable env}
  where
    f = Nonconsumable . entryAliases

addError :: (Located loc) => loc -> Notes -> Doc () -> CheckM ()
addError loc notes e = modify $ \s ->
  s {stateErrors = DL.snoc (stateErrors s) (TypeError (locOf loc) notes e)}

incCounter :: CheckM Int
incCounter =
  state $ \s -> (stateCounter s, s {stateCounter = stateCounter s + 1})

-- | An alias of a new internal name standing for an intermediate value, with
-- the reason it exists recorded for error messages.
internalAlias :: Name -> NameReason -> CheckM Alias
internalAlias desc reason = do
  v <- VName desc <$> incCounter
  modify $ \s -> s {stateNames = M.insert v reason $ stateNames s}
  pure $ AliasFree (v, [])

returnAliased :: Name -> SrcLoc -> CheckM ()
returnAliased name loc =
  addError loc mempty . withIndexLink "return-aliased" $
    "Fresh-declared return value is aliased to"
      <+> dquotes (prettyName name)
      <> ", which is not consumable."

-- | Returning a value for a fresh return type is equivalent to consuming it,
-- so a value with internal aliasing cannot be returned that way.
selfAliasedReturn :: (Located loc) => loc -> CheckM ()
selfAliasedReturn loc =
  addError loc mempty $
    "A fresh-declared component of the return value may have internal aliases,"
      </> "and so cannot be declared fresh."

freshReturnAliased :: SrcLoc -> CheckM ()
freshReturnAliased loc =
  addError loc mempty . withIndexLink "fresh-return-aliased" $
    "A fresh-declared component of the return value is aliased to some other component."

-- | Check that every component of a function result declared fresh may be.
checkReturnAlias :: SrcLoc -> [Pat ParamType] -> ResType -> TypeAliases -> CheckM ()
checkReturnAlias loc params rettp ret_als =
  forM_ (returnAliases rettp ret_als) $ \(u, t_als) ->
    when (u == Fresh) . mapM_ report $ unfreshness params shared t_als
  where
    shared = sharedLocations ret_als

    report (UnfreshAliases v) = returnAliased (baseName v) loc
    report UnfreshShared = freshReturnAliased loc
    report UnfreshSelf = selfAliasedReturn loc

    returnAliases (Scalar (Record ets1)) (Scalar (Record ets2)) =
      concat $ M.elems $ M.intersectionWith returnAliases ets1 ets2
    returnAliases expected got =
      [(freshness expected, got)]

unscope :: [VName] -> Aliases -> Aliases
unscope bound = S.map f
  where
    f (AliasBound (v, fs)) =
      if v `elem` bound
        then AliasFree (v, fs)
        else AliasBound (v, fs)
    f a = a

-- | Figure out the aliases of each bound name in a pattern.
matchPat :: Pat t -> TypeAliases -> DL.DList (VName, (t, TypeAliases))
matchPat (PatParens p _) t = matchPat p t
matchPat (TuplePat ps _) t
  | Just ts <- isTupleRecord t = mconcat $ zipWith matchPat ps ts
matchPat (RecordPat fs1 _) (Scalar (Record fs2)) =
  mconcat $
    zipWith
      matchPat
      (map snd (sortFields (M.fromList (map (first unLoc) fs1))))
      (map snd (sortFields fs2))
matchPat (Id v (Info t) _) als = DL.singleton (v, (t, als))
matchPat (PatAscription p _ _) t = matchPat p t
matchPat (PatConstr v _ ps _) (Scalar (Sum cs))
  | Just ts <- M.lookup v cs = mconcat $ zipWith matchPat ps ts
matchPat TuplePat {} _ = mempty
matchPat RecordPat {} _ = mempty
matchPat PatConstr {} _ = mempty
matchPat Wildcard {} _ = mempty
matchPat PatLit {} _ = mempty
matchPat (PatAttr _ p _) t = matchPat p t

bindingPat ::
  Pat StructType ->
  TypeAliases ->
  CheckM (a, TypeAliases) ->
  CheckM (a, TypeAliases)
bindingPat p t = fmap (second (second (unscope (patNames p)))) . local bind
  where
    bind env =
      env
        { envVtable =
            foldr (uncurry M.insert . f) (envVtable env) (matchPat p t)
        }
      where
        f (v, (_, als)) = (v, Consumable $ insertSelfAliases v als)

bindingParam :: Pat ParamType -> CheckM (a, TypeAliases) -> CheckM (a, TypeAliases)
bindingParam p m = do
  mapM_ (noConsumable . bitraverse_ checkExp pure) p
  second (second (unscope (patNames p))) <$> local bind m
  where
    bind env =
      env
        { envVtable =
            foldr (uncurry M.insert . f) (envVtable env) (patternMap p)
        }
    f (v, t)
      | diet t == Consume = (v, Consumable $ selfAliasType v t)
      | otherwise = (v, Nonconsumable $ selfAliasType v t)

bindingIdent :: Diet -> Ident StructType -> CheckM (a, TypeAliases) -> CheckM (a, TypeAliases)
bindingIdent d (Ident v (Info t) _) =
  fmap (second (second (unscope [v]))) . local bind
  where
    bind env = env {envVtable = M.insert v t' (envVtable env)}
    d' = case d of
      Consume -> Consumable
      Observe -> Nonconsumable
    t' = d' $ selfAliasType v t

bindingParams :: [Pat ParamType] -> CheckM (a, TypeAliases) -> CheckM (a, TypeAliases)
bindingParams params m =
  noConsumable $
    second (second (unscope (foldMap patNames params)))
      <$> foldr bindingParam m params

bindingLoopForm :: LoopFormBase Info VName -> CheckM (a, TypeAliases) -> CheckM (a, TypeAliases)
bindingLoopForm (For ident _) m = bindingIdent Observe ident m
bindingLoopForm (ForIn pat _) m = bindingParam pat' m
  where
    pat' = fmap (second (const Observe)) pat
bindingLoopForm While {} m = m

bindingFun :: VName -> TypeAliases -> CheckM a -> CheckM a
bindingFun v t = local $ \env ->
  env {envVtable = M.insert v (Nonconsumable t) (envVtable env)}

checkIfConsumed :: Loc -> Aliases -> CheckM ()
checkIfConsumed rloc als = do
  cons <- gets stateConsumed
  names <- gets stateNames
  let bad l = (l,) <$> deadIn cons l
      -- Mention the variables the programmer wrote before internal names.
      internal = (`M.member` names) . fst . fst
  forM_ (L.sortOn internal $ mapMaybe bad $ aliasLocs als) $ \(l, wloc) -> do
    v' <- describeLoc l
    addError rloc mempty . withIndexLink "use-after-consume" $
      "Using"
        <+> v'
        <> ", but this was consumed at"
          <+> pretty (locStrRel rloc wloc)
        <> ".  (Possibly through aliases.)"

consumed :: Consumed -> CheckM ()
consumed vs = modify $ \s -> s {stateConsumed = stateConsumed s <> vs}

consumeAliases :: Loc -> Aliases -> CheckM ()
consumeAliases loc als = do
  vtable <- asks envVtable
  let isBad v =
        case v `M.lookup` vtable of
          Just (Nonconsumable {}) -> True
          Just _ -> False
          Nothing -> True
      checkIfConsumable AliasFree {} = pure ()
      checkIfConsumable AliasSelf =
        addError
          loc
          mempty
          "Consuming a value that may have internal aliases."
      checkIfConsumable a
        | Just v <- aliasVar a,
          isBad v = do
            v' <- describeName v
            addError loc mempty . withIndexLink "not-consumable" $
              "Consuming" <+> v' <> ", which is not consumable."
      checkIfConsumable _ = pure ()
      -- Mention local variables before globals.
      global = maybe False (`M.notMember` vtable) . aliasVar
  mapM_ checkIfConsumable $ L.sortOn global $ S.toList als
  checkIfConsumed loc als
  consumed als'
  where
    als' = M.fromList $ map (,loc) $ aliasLocs als

-- | Observe the given name here and return its aliases.
observeVar :: Loc -> QualName VName -> StructType -> CheckM TypeAliases
observeVar loc qv t = do
  als <-
    asks $ \env ->
      maybe (isGlobal env) entryAliases $
        M.lookup v (envVtable env)
  checkIfConsumed loc (aliases als)
  pure als
  where
    v = qualLeaf qv

    -- The declared type of a global is what makes parametricity visible; if
    -- we cannot find it, fall back to the instantiated type, which amounts to
    -- assuming no parametricity at all.  An intrinsic aliases nothing.  See
    -- Note [Parametric results].
    isGlobal env
      | isIntrinsic v = second (const mempty) t
      | otherwise =
          let (tparams, decl) = fromMaybe ([], t) $ envGlobal env qv
           in notedAliases tparams decl $ globalAliases v tparams decl t

-- Capture any newly consumed locations that occur during the provided action.
contain :: CheckM a -> CheckM (a, Consumed)
contain m = do
  prev_cons <- gets stateConsumed
  x <- m
  new_cons <- gets $ (`M.difference` prev_cons) . stateConsumed
  modify $ \s -> s {stateConsumed = prev_cons}
  pure (x, new_cons)

-- | The two types are assumed to be approximately structurally equal,
-- but not necessarily regarding sizes.  Combines aliases and prefers
-- other information from first argument.
combineAliases :: TypeAliases -> TypeAliases -> TypeAliases
combineAliases (Array als1 et1 shape1) t2 =
  Array (als1 <> aliases t2) et1 shape1
combineAliases (Scalar (TypeVar als1 tv1 targs1)) t2 =
  Scalar $ TypeVar (als1 <> aliases t2) tv1 targs1
combineAliases t1 (Scalar (TypeVar als2 tv2 targs2)) =
  Scalar $ TypeVar (als2 <> aliases t1) tv2 targs2
combineAliases (Scalar (Record ts1)) (Scalar (Record ts2))
  | length ts1 == length ts2,
    L.sort (M.keys ts1) == L.sort (M.keys ts2) =
      Scalar $ Record $ M.intersectionWith combineAliases ts1 ts2
combineAliases
  (Scalar (Arrow als1 mn1 d1 pt1 (RetType dims1 rt1)))
  (Scalar (Arrow als2 _ _ _ (RetType _ _))) =
    Scalar (Arrow (als1 <> als2) mn1 d1 pt1 (RetType dims1 rt1))
combineAliases (Scalar (Sum cs1)) (Scalar (Sum cs2))
  | length cs1 == length cs2,
    L.sort (M.keys cs1) == L.sort (M.keys cs2) =
      Scalar $ Sum $ M.intersectionWith (zipWith combineAliases) cs1 cs2
combineAliases (Scalar (Prim t)) _ = Scalar $ Prim t
combineAliases t1 t2 =
  error $ "combineAliases invalid args: " ++ show (t1, t2)

-- | The locations that occur in more than one component of a value.  A
-- component aliasing any of them cannot be fresh.
sharedLocations :: TypeAliases -> S.Set Location
sharedLocations =
  M.keysSet
    . M.filter (> 1)
    . M.fromListWith (+)
    . concatMap (map (,1 :: Int) . S.toList . S.fromList . aliasLocs)
    . aliasParts

-- | Is this location entirely within a part of a parameter that is consumed?
consumedParamLoc :: [Pat ParamType] -> Location -> Bool
consumedParamLoc params (v, fs) =
  case maybe [] (filter ((fs `L.isPrefixOf`) . fst) . leaves) $
    lookup v (foldMap patternMap params) of
    [] -> False
    within -> all (consumable . snd) within
  where
    consumable (Array d _ _) = d == Consume
    consumable (Scalar (TypeVar d _ _)) = d == Consume
    consumable (Scalar Arrow {}) = False
    consumable _ = True

-- | A reason why a component of a function result cannot be fresh.
data Unfresh
  = -- | It aliases this variable, which is in scope and not a consumed
    -- parameter.
    UnfreshAliases VName
  | -- | It aliases a location that some other component also aliases.
    UnfreshShared
  | -- | It may have internal aliasing.
    UnfreshSelf

-- | Why a component of the result of a function with these parameters cannot
-- be fresh, given the 'sharedLocations' of the whole result.  The component
-- may be fresh exactly when there is no reason.  See Note
-- [Locations].
unfreshness :: [Pat ParamType] -> S.Set Location -> TypeAliases -> [Unfresh]
unfreshness params shared t_als =
  [UnfreshShared | any (`S.member` shared) (aliasLocs (aliases t_als))]
    <> [UnfreshSelf | selfAliased (aliases t_als)]
    <> map (UnfreshAliases . fst) (filter (not . consumedParamLoc params) in_scope)
  where
    -- Mention the parameters before other variables.
    in_scope =
      L.sortOn ((`notElem` foldMap patNames params) . fst) . nubOrd . aliasLocs $
        S.filter isBoundAlias $
          arrayAliases t_als

arrayAliases :: TypeAliases -> Aliases
arrayAliases (Array als _ _) = als
arrayAliases (Scalar Prim {}) = mempty
arrayAliases (Scalar (Record fs)) = foldMap arrayAliases fs
arrayAliases (Scalar (TypeVar als _ _)) = als
arrayAliases (Scalar Arrow {}) = mempty
arrayAliases (Scalar (Sum fs)) =
  mconcat $ concatMap (map arrayAliases) $ M.elems fs

-- | The aliases of the free local variables captured by a closure, plus any
-- globals that its result aliases, which it may return.
closureAliases :: Exp -> TypeAliases -> CheckM Aliases
closureAliases e body_als = do
  vtable <- asks envVtable
  free_bound <- boundFreeInExp e
  -- A function's own note comes from its body; 'AliasSelf' is not an alias of
  -- anything, so the usual global/local distinction does not apply to it.
  let isGlobal AliasFree {} = False
      isGlobal AliasSelf = True
      isGlobal a = maybe False (`M.notMember` vtable) $ aliasVar a
  pure $
    foldMap aliases (M.elems free_bound)
      <> S.filter isGlobal (aliases body_als)

overlapCheck :: (Pretty src, Pretty ve) => Loc -> (src, TypeAliases) -> (ve, TypeAliases) -> CheckM ()
overlapCheck loc (src, src_als) (ve, ve_als) =
  when (aliases src_als `overlaps` aliases ve_als) $
    addError loc mempty $
      "Source array for in-place update"
        </> indent 2 (pretty src)
        </> "might alias update value"
        </> indent 2 (pretty ve)
        </> "Hint: use"
        <+> dquotes "copy"
        <+> "to remove aliases from the value."

-- | 'setMode' does not look inside an arrow, but when we return a function, the
-- freshness of *its* return type has already been inferred when checking the
-- lambda.  So go past all the arrows and set the freshness appropriately - note
-- that for a function the given 'Freshness' is therefore ignored, as the answer
-- is already recorded in the type.
withArrowRet :: ResType -> TypeAliases -> Freshness -> ResType
withArrowRet
  (Scalar (Arrow u pn d pt (RetType ext t1)))
  (Scalar (Arrow _ _ _ _ (RetType _ t2)))
  _ =
    Scalar . Arrow u pn d pt . RetType ext $ go t1 t2
    where
      go (Scalar (Record fs1)) (Scalar (Record fs2)) =
        Scalar $ Record $ M.intersectionWith go fs1 fs2
      go (Scalar (Sum cs1)) (Scalar (Sum cs2)) =
        Scalar $ Sum $ M.intersectionWith (zipWith go) cs1 cs2
      go
        (Scalar (Arrow u' pn' d' pt' (RetType ext' a)))
        (Scalar (Arrow _ _ _ _ (RetType _ b))) =
          Scalar . Arrow u' pn' d' pt' . RetType ext' $ go a b
      go a b = a `setMode` freshness b
withArrowRet t _ u = t `setMode` u

inferReturnFreshness :: [Pat ParamType] -> ResType -> TypeAliases -> ResType
inferReturnFreshness [] ret _ = ret `setMode` Nonfresh
inferReturnFreshness params ret ret_als = delve ret ret_als
  where
    shared = sharedLocations ret_als
    delve (Scalar (Record fs1)) (Scalar (Record fs2)) =
      Scalar $ Record $ M.intersectionWith delve fs1 fs2
    delve (Scalar (Sum cs1)) (Scalar (Sum cs2)) =
      Scalar $ Sum $ M.intersectionWith (zipWith delve) cs1 cs2
    delve t t_als =
      withArrowRet t t_als $
        if null (unfreshness params shared t_als) then Fresh else Nonfresh

checkSubExps :: (ASTMappable e) => e -> CheckM e
checkSubExps = astMap identityMapper {mapOnExp = fmap fst . checkExp}

noAliases :: Exp -> CheckM (Exp, TypeAliases)
noAliases e = do
  e' <- checkSubExps e
  pure (e', unknownAliases (typeOf e))

-- | The aliases of each leaf of a value.
aliasParts :: TypeAliases -> [Aliases]
aliasParts = map (aliases . snd) . leaves

-- | Are the components of this value pairwise disjoint?
separated :: TypeAliases -> Bool
separated = S.null . sharedLocations

noSelfAliases :: Loc -> TypeAliases -> CheckM ()
noSelfAliases loc t =
  unless (separated t) $
    addError loc mempty . withIndexLink "self-aliasing-arg" $
      "Argument passed for consuming parameter is self-aliased."

-- | The leaf at the given path, if there is one.
componentAt :: Path -> TypeAliases -> Maybe TypeAliases
componentAt fs = lookup fs . leaves

-- | The locations in the alias set of a location: those of the leaf at that
-- path of the variable's entry in the vtable.  Empty for a location that is
-- not a leaf of a variable in scope.
aliasOf :: M.Map VName (Entry TypeAliases) -> Location -> [Location]
aliasOf vtable (v, fs) =
  maybe [] (aliasLocs . aliases) $ componentAt fs . entryAliases =<< M.lookup v vtable

-- | The aliases of the components of an argument that a parameter of this type
-- consumes.
consumedAliasesOf :: ParamType -> TypeAliases -> Aliases
consumedAliasesOf (Scalar (Record fs1)) (Scalar (Record fs2)) =
  mconcat $ M.elems $ M.intersectionWith consumedAliasesOf fs1 fs2
consumedAliasesOf p_t t_als
  | diet p_t == Consume = aliases t_als
  | otherwise = mempty

-- | Check an argument, given the aliases of the function being applied and the
-- arguments evaluated before this one.  What the argument consumes may alias
-- neither.
checkArg :: Aliases -> [(Exp, TypeAliases)] -> ParamType -> Exp -> CheckM (Exp, TypeAliases)
checkArg f_als prev p_t e = do
  ((e', e_als), e_cons) <- contain $ checkExp e
  consumed e_cons
  let e_t = typeOf e'
  when (e_cons /= mempty && not (orderZero e_t)) $
    addError (locOf e) mempty . withIndexLink "consuming-argument" $
      "Argument of functional type"
        </> indent 2 (pretty e_t)
        </> "contains consumption, which is not allowed."
  when (diet p_t == Consume) $ do
    noSelfAliases (locOf e) e_als
    let cons_als = consumedAliasesOf p_t e_als
    consumeAliases (locOf e) cons_als
    when (cons_als `overlaps` f_als) . addError (locOf e) mempty $
      "Argument is consumed, but aliases the function being applied."
    case find ((cons_als `overlaps`) . aliases . snd) prev of
      Nothing -> pure ()
      Just (prev_arg, prev_als) -> do
        shared <- describeShared $ aliasLocs $ cons_als `S.intersection` aliases prev_als
        addError (locOf e) mempty $
          "Argument is consumed, but aliases"
            </> indent 2 shared
            </> "which is also aliased by other argument"
            </> indent 2 (pretty prev_arg)
            </> "at"
            <+> pretty (locTextRel (locOf e) (locOf prev_arg))
            <> "."
  pure (e', e_als)
  where
    -- Name a variable the programmer wrote if there is one.
    describeShared locs = do
      names <- gets stateNames
      case L.partition ((`M.notMember` names) . fst) locs of
        ((v, fs) : _, _) -> pure $ prettyAlias v fs
        ([], l : _) -> describeLoc l
        ([], []) -> pure mempty

-- | Does any component of the value that this type ultimately produces satisfy
-- the predicate?  Function types are followed to their (curried) result, as the
-- only way to obtain a value from a function is to apply it.
anyResultComponent :: (ResType -> Bool) -> ResType -> Bool
anyResultComponent p (Scalar (Arrow _ _ _ _ (RetType _ t))) = anyResultComponent p t
anyResultComponent p (Scalar (Record fs)) = any (anyResultComponent p) fs
anyResultComponent p (Scalar (Sum cs)) = any (any (anyResultComponent p)) cs
anyResultComponent p t = p t

-- | Can a value of this declared type produce, when its function components
-- are applied, a value whose internal aliasing we cannot see?  That is so
-- exactly when some component of what it produces is a nonfresh abstract type
-- that is not one of the type parameters it is polymorphic in: it must then
-- have manufactured that value, rather than been handed it.  Intrinsic types
-- (notably accumulators) are exempt, as the compiler does know their
-- representation, and they have no components that could alias each other.  See
-- Note [Parametric results].
manufacturesAbstract :: [TypeParam] -> TypeBase Size u -> Bool
manufacturesAbstract tparams = anyResultComponent manufactured . toRes Nonfresh
  where
    tparams' = [v | TypeParamType _ v _ <- tparams]
    manufactured (Scalar (TypeVar u t _)) =
      u == Nonfresh
        && not (isIntrinsic (qualLeaf t))
        && qualLeaf t `notElem` tparams'
    manufactured _ = False

-- | Note on every function component of a type that applying it may yield a
-- value with internal aliasing.  See Note [Parametric results].
noteSelfAliases :: TypeAliases -> TypeAliases
noteSelfAliases (Scalar (Arrow als mn d pt rt)) =
  Scalar $ Arrow (S.insert AliasSelf als) mn d pt rt
noteSelfAliases (Scalar (Record fs)) = Scalar $ Record $ fmap noteSelfAliases fs
noteSelfAliases (Scalar (Sum cs)) = Scalar $ Sum $ (fmap . fmap) noteSelfAliases cs
noteSelfAliases t = t

-- | Note on the function components of a value that applying them may produce a
-- value with internal aliasing, when the given declared type says they may.
-- See Note [Parametric results].
notedAliases :: [TypeParam] -> TypeBase Size u -> TypeAliases -> TypeAliases
notedAliases tparams decl
  | manufacturesAbstract tparams decl = noteSelfAliases
  | otherwise = id

selfAliasType :: VName -> TypeBase Size o -> TypeAliases
selfAliasType v = insertSelfAliases v . unknownAliases

-- | The aliases to assume for a value whose provenance we know nothing about:
-- none at all, except what its own type says it may manufacture.  This is
-- 'notedAliases' with no type parameters to exploit.  See Note [Parametric
-- results].
unknownAliases :: TypeBase Size u -> TypeAliases
unknownAliases t = notedAliases [] t $ second (const mempty) t

-- | @returnType appres ret_type arg_diet arg_type@ gives result of applying
-- an argument the given types to a function with the given return
-- type, consuming the argument with the given diet.
returnType :: Aliases -> ResType -> Diet -> TypeAliases -> TypeAliases
returnType _ (Array Fresh et shape) _ _ =
  Array mempty et shape
returnType appres (Array Nonfresh et shape) Consume _ =
  Array appres et shape
returnType appres (Array Nonfresh et shape) Observe arg =
  Array (appres <> aliases arg) et shape
returnType _ (Scalar (TypeVar Fresh t targs)) _ _ =
  Scalar $ TypeVar mempty t targs
returnType appres (Scalar (TypeVar Nonfresh t targs)) Consume _ =
  Scalar $ TypeVar appres t targs
returnType appres (Scalar (TypeVar Nonfresh t targs)) Observe arg =
  Scalar $ TypeVar (appres <> aliases arg) t targs
returnType appres (Scalar (Record fs)) d arg =
  Scalar $ Record $ fmap (\et -> returnType appres et d arg) fs
returnType _ (Scalar (Prim t)) _ _ =
  Scalar $ Prim t
returnType appres (Scalar (Arrow _ v pd t1 (RetType dims t2))) Consume _ =
  Scalar $ Arrow appres v pd t1 $ RetType dims t2
returnType appres (Scalar (Arrow _ v pd t1 (RetType dims t2))) Observe arg =
  Scalar $ Arrow (appres <> aliases arg) v pd t1 $ RetType dims t2
returnType appres (Scalar (Sum cs)) d arg =
  Scalar $ Sum $ (fmap . fmap) (\et -> returnType appres et d arg) cs

applyArg :: TypeAliases -> TypeAliases -> TypeAliases
applyArg (Scalar (Arrow closure_als _ d _ (RetType _ rettype))) arg_als =
  returnType closure_als rettype d arg_als
applyArg _ arg_als = arg_als

applyLoopArg :: Aliases -> ParamType -> TypeAliases -> ResType -> TypeAliases
applyLoopArg appres (Scalar (Record pfs)) (Scalar (Record afs)) (Scalar (Record rfs)) =
  Scalar . Record $
    M.mapWithKey
      (\k p_t -> applyLoopArg appres p_t (afs M.! k) (rfs M.! k))
      pfs
applyLoopArg appres p_t arg_als rettype =
  returnType appres rettype (diet p_t) arg_als

boundFreeInExp :: Exp -> CheckM (M.Map VName TypeAliases)
boundFreeInExp e = do
  vtable <- asks envVtable
  pure $
    M.mapMaybe (fmap entryAliases) . M.fromSet (`M.lookup` vtable) $
      fvVars (freeInExp e)

-- Loops are tricky because we want to infer the diets of their parameters.
-- This is pretty unusual: we do not do this for ordinary functions.
type Loop = (Pat ParamType, LoopInitBase Info VName, LoopFormBase Info VName, Exp)

-- | Mark bindings of consumed names as Consume, except those under a
-- 'PatAscription', which are left unchanged.
updateParamDiet :: (VName -> Bool) -> Pat ParamType -> Pat ParamType
updateParamDiet cons = recurse
  where
    recurse (Wildcard (Info t) wloc) =
      Wildcard (Info $ t `setMode` Observe) wloc
    recurse (PatParens p ploc) =
      PatParens (recurse p) ploc
    recurse (PatAttr attr p ploc) =
      PatAttr attr (recurse p) ploc
    recurse (Id name (Info t) iloc)
      | cons name =
          let t' = t `setMode` Consume
           in Id name (Info t') iloc
      | otherwise =
          let t' = t `setMode` Observe
           in Id name (Info t') iloc
    recurse (TuplePat pats ploc) =
      TuplePat (map recurse pats) ploc
    recurse (RecordPat fs ploc) =
      RecordPat (map (fmap recurse) fs) ploc
    recurse (PatAscription p t ploc) =
      PatAscription p t ploc
    recurse p@PatLit {} = p
    recurse (PatConstr n t ps ploc) =
      PatConstr n t (map recurse ps) ploc

-- | Infer which loop parameters are consumed, and check what the body returns
-- for them.  A parameter is consumed if the body consumes it, or if the value
-- returned for a consumed parameter aliases it, as that value is consumed in
-- the next iteration.  See Note [Locations].
convergeLoopParam :: Loc -> Pat ParamType -> S.Set VName -> TypeAliases -> CheckM (Pat ParamType)
convergeLoopParam loop_loc param body_cons body_als
  | body_cons' /= body_cons = convergeLoopParam loop_loc param body_cons' body_als
  | otherwise = do
      checkLoopResult loop_loc param' body_als
      pure param'
  where
    param' = updateParamDiet (`S.member` body_cons) param
    returned = toList $ matchPat param' body_als
    cons_als = foldMap (aliases . snd . snd) $ filter ((== Consume) . diet . fst . snd) returned
    body_cons' = body_cons <> S.filter (`elem` patNames param) (aliasVars cons_als)

-- | Check the values a loop body returns for its consumed parameters, as the
-- arguments of a call that consumes them: each must be fresh, as the result of
-- a function with the loop parameters as its parameters would have to be.
checkLoopResult :: Loc -> Pat ParamType -> TypeAliases -> CheckM ()
checkLoopResult loop_loc param body_als =
  forM_ (matchPat param body_als) $ \(v, (t, als)) ->
    when (diet t == Consume) . mapM_ (report v) $ unfreshness [param] shared als
  where
    shared = sharedLocations body_als
    what v = "Return value for consuming loop parameter" <+> dquotes (prettyName v)
    report v (UnfreshAliases w) =
      addError loop_loc mempty . withIndexLink "consuming-loop-param-aliases" $
        what v <+> "aliases" <+> dquotes (prettyName w) <> "."
    report v UnfreshShared =
      addError loop_loc mempty . withIndexLink "aliases-previously-returned" $
        what v <+> "aliases another returned value."
    report v UnfreshSelf =
      addError loop_loc mempty $
        what v <+> "may have internal aliases."

checkLoop :: Loc -> Loop -> CheckM (Loop, TypeAliases)
checkLoop loop_loc (param, arg, form, body) = do
  form' <- checkSubExps form
  -- We pretend that every part of the loop parameter has a consuming
  -- diet, as we need to allow consumption in the body, which we then
  -- use to infer the proper diet of the parameter.
  ((body', body_cons), body_als) <-
    noConsumable
      . bindingParam (updateParamDiet (const True) param)
      . bindingLoopForm form'
      $ do
        ((body', body_als), body_cons) <- contain $ checkExp body
        pure ((body', body_cons), body_als)
  param' <- convergeLoopParam loop_loc param (S.map fst (M.keysSet body_cons)) body_als

  let param_t = patternType param'
  ((arg', arg_als), arg_cons) <- case arg of
    LoopInitImplicit (Info e) ->
      contain $ first (LoopInitImplicit . Info) <$> checkArg mempty [] param_t e
    LoopInitExplicit e ->
      contain $ first LoopInitExplicit <$> checkArg mempty [] param_t e
  consumed arg_cons

  let checkFree what e = do
        free_bound <- boundFreeInExp e

        let bad = any (isJust . deadIn arg_cons) . aliasLocs . aliases . snd
        forM_ (filter bad $ M.toList free_bound) $ \(v, _) -> do
          v' <- describeVar v
          addError loop_loc mempty $
            what
              <+> "uses"
              <+> v'
              <> " (or an alias),"
                </> "but this is consumed by the initial loop argument."

  checkFree "Loop body" body

  case form of
    While cond -> checkFree "Loop condition" cond
    _ -> pure ()

  loop_al <- internalAlias "internal_loop_result" $ NameLoopRes $ srclocOf loop_loc

  let loop_als =
        applyLoopArg
          (S.singleton loop_al)
          param_t
          arg_als
          (paramToRes param_t)
  pure
    ( (param', arg', form', body'),
      loop_als `combineAliases` body_als
    )

-- | The type of a global applied to arguments of the given types, with what
-- parametricity tells us about the freshness of the result recorded in it.
-- Only an application that supplies every parameter of the type is refined.
-- See Note [Parametric results].
parametricFreshness ::
  QualName VName ->
  StructType ->
  [StructType] ->
  CheckM StructType
parametricFreshness qn ftype argtypes = do
  globals <- asks envGlobal
  pure $ fromMaybe ftype $ do
    (tparams, decl) <- globals qn
    (param_ts, res) <- funParts decl
    guard $ length argtypes == length param_ts
    i <- resultFromParam tparams param_ts res
    x <- case res of
      Scalar (TypeVar _ v _) -> Just $ qualLeaf v
      _ -> Nothing
    guard $ constructsFresh $ argtypes !! i
    Just $ freshenOccurrences x decl ftype

-- | Peel the parameters off a function type, returning their types (in order)
-- and the type of the final result.  'Nothing' for a non-function type.  This
-- is 'unfoldFunType' except that it preserves the freshness of the result,
-- which is exactly what we are asking about here.
funParts :: TypeBase Size u -> Maybe ([StructType], ResType)
funParts (Scalar (Arrow _ _ _ pt (RetType _ t))) = Just $ go [pt] t
  where
    go ps (Scalar (Arrow _ _ _ pt' (RetType _ t'))) = go (pt' : ps) t'
    go ps t' = (reverse ps, t')
funParts _ = Nothing

-- | If the result of a function with this declared type can only be the result
-- of applying one of its own parameters, the position of that parameter.  That
-- is the case when the result is a type parameter which occurs in exactly one
-- of the parameters, and there only as the result of a function: the only way
-- to obtain a value of an unknown type is to be handed one, and no parameter
-- but that one holds any.
resultFromParam :: [TypeParam] -> [StructType] -> ResType -> Maybe Int
resultFromParam tparams params res
  | Scalar (TypeVar Nonfresh v _) <- res,
    qualLeaf v `elem` [pv | TypeParamType _ pv _ <- tparams],
    [(i, pt)] <- filter (S.member (qualLeaf v) . typeVars . snd) $ zip [0 ..] params,
    isFunResult (qualLeaf v) pt =
      Just i
  | otherwise = Nothing
  where
    isFunResult v (Scalar (Arrow _ _ _ _ (RetType _ t))) = isResult v t
    isFunResult _ _ = False
    isResult v (Scalar (Arrow _ _ _ _ (RetType _ t))) = isResult v t
    isResult v (Scalar (TypeVar _ t _)) = qualLeaf t == v
    isResult _ _ = False

-- | Does applying this function construct its result freshly?  That is so when
-- every part of its (curried) result is fresh or primitive.  Requiring the
-- result to be order zero keeps us from claiming that a closure over the other
-- arguments aliases nothing.
constructsFresh :: TypeBase Size u -> Bool
constructsFresh t
  | Just (_, rt) <- funParts t = orderZero rt && allFresh rt
  | otherwise = False
  where
    allFresh (Scalar (Record fs)) = all allFresh fs
    allFresh (Scalar (Sum cs)) = all (all allFresh) cs
    allFresh (Scalar Prim {}) = True
    allFresh (Scalar (TypeVar u _ _)) = u == Fresh
    allFresh (Array u _ _) = u == Fresh
    allFresh (Scalar Arrow {}) = False

-- | Mark as fresh every return-type slot that the declared type fills with the
-- given type parameter.  Both the result of the function and the result of the
-- parameter it came from must say so, or the instantiation would not be
-- well-typed.
freshenOccurrences :: VName -> StructType -> StructType -> StructType
freshenOccurrences x = onStruct
  where
    onStruct
      (Scalar (Arrow _ _ _ sa (RetType _ sr)))
      (Scalar (Arrow u pn d ta (RetType ext tr))) =
        Scalar $ Arrow u pn d (onStruct sa ta) $ RetType ext (onRes sr tr)
    onStruct _ t = t

    onRes (Scalar (TypeVar _ v _)) tr
      | qualLeaf v == x = tr `setMode` Fresh
    onRes
      (Scalar (Arrow _ _ _ sa (RetType _ sr)))
      (Scalar (Arrow u pn d ta (RetType ext tr))) =
        Scalar $ Arrow u pn d (onStruct sa ta) $ RetType ext (onRes sr tr)
    onRes _ tr = tr

checkFuncall ::
  (Foldable f) =>
  SrcLoc ->
  Maybe (QualName VName) ->
  TypeAliases ->
  f TypeAliases ->
  CheckM TypeAliases
checkFuncall loc fname f_als arg_als = do
  app_al <- internalAlias "internal_app_result" $ NameAppRes fname loc
  pure $ foldl applyArg (second (S.insert app_al) f_als) arg_als

-- | Join the results of the branches of a branching expression (described by
-- the string), given everything consumed by any of them.  An alias survives if it and everything it aliases
-- is still alive; the rest are consumed.  If the components of some branch's
-- result overlap, the components of the joined result are tied together by a
-- fresh name.  See Note [Locations].
joinBranches :: T.Text -> Loc -> Consumed -> NE.NonEmpty TypeAliases -> CheckM TypeAliases
joinBranches what loc all_cons branches = do
  vtable <- asks envVtable
  let alive = isNothing . deadIn all_cons
      keep a = case aliasLoc a of
        Nothing -> True
        Just l -> alive l && all alive (aliasOf vtable l)
      t = foldl1 combineAliases branches
      dropped = S.filter (not . keep) $ aliases t
  consumed $ all_cons <> M.fromList (map (,loc) (aliasLocs dropped))
  tie <-
    if all separated branches
      then pure id
      else S.insert <$> internalAlias "internal_branch_result" (NameBranchRes what (srclocOf loc))
  pure $ second (tie . S.filter keep) t

checkExp :: Exp -> CheckM (Exp, TypeAliases)
-- First we have the complicated cases.

--
checkExp (AppExp (Apply f args loc) appres) = do
  f_fresh <- case f of
    Var qn (Info t) floc -> do
      t' <- parametricFreshness qn t $ map (typeOf . snd) $ NE.toList args
      pure $ Var qn (Info t') floc
    _ -> pure f
  (f', f_als) <- checkExp f_fresh
  (args', args_als) <- NE.unzip <$> checkArgs (aliases f_als) (diets $ toRes Nonfresh f_als) args
  res_als <- checkFuncall loc (fname f) f_als args_als
  pure (AppExp (Apply f' args' loc) appres, res_als)
  where
    fname (Var v _ _) = Just v
    fname (AppExp (Apply e _ _) _) = fname e
    fname _ = Nothing
    checkArg' f_als prev d (Info p, e) = do
      (e', e_als) <- checkArg f_als prev (second (const d) (typeOf e)) e
      pure ((Info p, e'), e_als)

    diets (Scalar (Arrow _ _ d _ (RetType _ rt))) =
      d : diets rt
    diets _ = repeat Observe

    checkArgs f_als ds (x NE.:| args') = do
      let (d, ds') = fromMaybe (Observe, []) $ L.uncons ds
      -- Note Futhark uses right-to-left evaluation of applications.
      args'' <- maybe (pure []) (fmap NE.toList . checkArgs f_als ds') $ NE.nonEmpty args'
      (x', x_als) <- checkArg' f_als (map (first snd) args'') d x
      pure $ (x', x_als) NE.:| args''

--
checkExp (AppExp (Loop sparams pat loopinit form body loc) appres) = do
  ((pat', loopinit', form', body'), als) <-
    checkLoop (locOf loc) (pat, loopinit, form, body)
  pure
    ( AppExp (Loop sparams pat' loopinit' form' body' loc) appres,
      als
    )

--
checkExp (AppExp (LetPat sizes p e body loc) appres) = do
  ((e', e_als), e_cons) <- contain $ checkExp e
  consumed e_cons
  let e_t = typeOf e'
  when (e_cons /= mempty && not (orderZero e_t)) $
    addError (locOf e) mempty . withIndexLink "contains-consumption" $
      "Let-bound expression of higher-order type"
        </> indent 2 (pretty e_t)
        </> "contains consumption, which is not allowed."
  bindingPat p e_als $ do
    (body', body_als) <- checkExp body
    pure
      ( AppExp (LetPat sizes p e' body' loc) appres,
        body_als
      )

--
checkExp (AppExp (If cond te fe loc) appres) = do
  (cond', _) <- checkExp cond
  ((te', te_als), te_cons) <- contain $ checkExp te
  ((fe', fe_als), fe_cons) <- contain $ checkExp fe
  comb_als <- joinBranches "if-expression" (locOf loc) (te_cons <> fe_cons) $ te_als NE.:| [fe_als]
  pure
    ( AppExp (If cond' te' fe' loc) appres,
      appResType (unInfo appres) `setAliases` mempty `combineAliases` comb_als
    )

--
checkExp (AppExp (Match cond cs loc) appres) = do
  (cond', cond_als) <- checkExp cond
  ((cs', cs_als), cs_cons) <-
    first NE.unzip . NE.unzip <$> mapM (checkCase cond_als) cs
  comb_als <- joinBranches "match-expression" (locOf loc) (fold cs_cons) cs_als
  pure
    ( AppExp (Match cond' cs' loc) appres,
      appResType (unInfo appres) `setAliases` mempty `combineAliases` comb_als
    )
  where
    checkCase cond_als (CasePat p body caseloc) =
      contain $ bindingPat p cond_als $ do
        (body', body_als) <- checkExp body
        pure (CasePat p body' caseloc, body_als)

--
checkExp (AppExp (LetFun fname (typarams, params, retdecl, Info (RetType ext ret), funbody) letbody loc) appres) = do
  ((ret', funbody'), ftype) <- bindingParams params $ do
    -- Throw away the consumption - it can refer only to the parameters
    -- anyway.
    ((funbody', funbody_als), _body_cons) <- contain $ checkExp funbody
    checkReturnAlias loc params ret funbody_als
    als <- closureAliases funbody funbody_als
    let ret' = maybe (inferReturnFreshness params ret funbody_als) (const ret) retdecl
        ftype = funType params (RetType ext ret') `setAliases` als
    pure ((ret', funbody'), ftype)
  (letbody', letbody_als) <- bindingFun (fst fname) ftype $ checkExp letbody
  pure
    ( AppExp (LetFun fname (typarams, params, retdecl, Info (RetType ext ret'), funbody') letbody' loc) appres,
      letbody_als
    )

--
checkExp (AppExp (BinOp (op, oploc) (Info op_t) (x, xp) (y, yp) loc) appres) = do
  op_t' <- parametricFreshness op op_t [typeOf x, typeOf y]
  op_als <- observeVar (locOf oploc) op op_t'
  let (_, at1) : (_, at2) : _ = fst $ unfoldFunType op_als
  (x', x_als) <- checkArg (aliases op_als) [] at1 x
  (y', y_als) <- checkArg (aliases op_als) [(x', x_als)] at2 y
  res_als <- checkFuncall loc (Just op) op_als [x_als, y_als]
  pure
    ( AppExp (BinOp (op, oploc) (Info op_t') (x', xp) (y', yp) loc) appres,
      res_als
    )

--
checkExp e@(Lambda params body te (Info (RetType ext ret)) loc) =
  bindingParams params $ do
    -- Throw away the consumption - it can refer only to the parameters
    -- anyway.
    ((body', body_als), _body_cons) <- contain $ checkExp body
    checkReturnAlias loc params ret body_als
    als <- closureAliases e body_als
    let ret' = maybe (inferReturnFreshness params ret body_als) (const ret) te
        ftype = funType params (RetType ext ret') `setAliases` als
    pure
      ( Lambda params body' te (Info (RetType ext ret')) loc,
        ftype
      )

--
checkExp (AppExp (LetWith dst src steps ve body loc) appres) = do
  steps' <- mapM checkStep steps
  (ve', ve_als) <- checkExp ve
  src_als <- observeVar (locOf src) (qualName (identName src)) (unInfo $ identType src)

  let hasIndex = any isIndex steps

  when hasIndex $ do
    overlapCheck (locOf ve) (src, src_als) (ve', ve_als)
    consumeAliases (locOf loc) $ aliases src_als

  (body', body_als) <- bindingIdent Consume dst $ checkExp body
  pure (AppExp (LetWith dst src steps' ve' body' loc) appres, body_als)
  where
    isIndex UpdateStepSlice {} = True
    isIndex _ = False
    checkStep (UpdateStepSlice slice) = UpdateStepSlice <$> checkSubExps slice
    checkStep (UpdateStepField f) = pure $ UpdateStepField f
--
checkExp (Update src steps ve t loc) = do
  steps' <- mapM checkStep steps
  (ve', ve_als) <- checkExp ve
  (src', src_als) <- checkExp src
  let hasIndex = any isIndex steps
  res_als <-
    if hasIndex
      then do
        overlapCheck (locOf ve) (src', src_als) (ve', ve_als)
        consumeAliases (locOf loc) $ aliases src_als
        pure $ second (const mempty) src_als
      else pure $ updateAliases src_als steps ve_als
  pure (Update src' steps' ve' t loc, res_als)
  where
    isIndex UpdateStepSlice {} = True
    isIndex _ = False
    checkStep (UpdateStepSlice slice) = UpdateStepSlice <$> checkSubExps slice
    checkStep (UpdateStepField f) = pure $ UpdateStepField f

-- Cases that simply propagate aliases directly.
checkExp (Var v (Info t) loc) = do
  als <- observeVar (locOf loc) v t
  checkIfConsumed (locOf loc) (aliases als)
  pure (Var v (Info t) loc, als)
checkExp (OpSection v (Info t) loc) = do
  als <- observeVar (locOf loc) v t
  checkIfConsumed (locOf loc) (aliases als)
  pure (OpSection v (Info t) loc, als)
checkExp (OpSectionLeft op ftype arg arginfo retinfo loc) = do
  let (_, Info (pn, pt2)) = arginfo
      (Info ret, _) = retinfo
  als <- observeVar (locOf loc) op (unInfo ftype)
  (arg', arg_als) <- checkExp arg
  pure
    ( OpSectionLeft op ftype arg' arginfo retinfo loc,
      Scalar $ Arrow (aliases arg_als <> aliases als) pn (diet pt2) (toStruct pt2) ret
    )
checkExp (OpSectionRight op ftype arg arginfo retinfo loc) = do
  let (Info (pn, pt2), _) = arginfo
      Info ret = retinfo
  als <- observeVar (locOf loc) op (unInfo ftype)
  (arg', arg_als) <- checkExp arg
  pure
    ( OpSectionRight op ftype arg' arginfo retinfo loc,
      Scalar $ Arrow (aliases arg_als <> aliases als) pn (diet pt2) (toStruct pt2) ret
    )
checkExp (UpdateSection steps t loc) = do
  steps' <- mapM checkStep steps
  pure (UpdateSection steps' t loc, unknownAliases (unInfo t))
  where
    checkStep (UpdateStepField f) = pure $ UpdateStepField f
    checkStep (UpdateStepSlice slice) = UpdateStepSlice <$> checkSubExps slice
checkExp (Coerce e te t loc) = do
  (e', e_als) <- checkExp e
  pure (Coerce e' te t loc, e_als)
checkExp (Ascript e te loc) = do
  (e', e_als) <- checkExp e
  pure (Ascript e' te loc, e_als)
checkExp (AppExp (Index v slice loc) appres) = do
  (v', v_als) <- checkExp v
  slice' <- checkSubExps slice
  pure
    ( AppExp (Index v' slice' loc) appres,
      appResType (unInfo appres) `setAliases` aliases v_als
    )
checkExp (Assert e1 e2 t loc) = do
  (e1', _) <- checkExp e1
  (e2', e2_als) <- checkExp e2
  pure (Assert e1' e2' t loc, e2_als)
checkExp (Parens e loc) = do
  (e', e_als) <- checkExp e
  pure (Parens e' loc, e_als)
checkExp (QualParens v e loc) = do
  (e', e_als) <- checkExp e
  pure (QualParens v e' loc, e_als)
checkExp (Attr attr e loc) = do
  (e', e_als) <- checkExp e
  pure (Attr attr e' loc, e_als)
checkExp (Project name e t loc) = do
  (e', e_als) <- checkExp e
  pure
    ( Project name e' t loc,
      case e_als of
        Scalar (Record fs)
          | Just name_als <- M.lookup name fs -> name_als
        _ -> error $ "checkExp Project: bad type " <> prettyString e_als
    )
checkExp (TupLit es loc) = do
  (es', es_als) <- mapAndUnzipM checkExp es
  pure (TupLit es' loc, Scalar $ tupleRecord es_als)
checkExp (Constr name es t loc) = do
  (es', es_als) <- mapAndUnzipM checkExp es
  pure
    ( Constr name es' t loc,
      case unInfo t of
        Scalar (Sum cs) ->
          Scalar . Sum . M.insert name es_als $
            M.map (map (`setAliases` mempty)) cs
        t' -> error $ "checkExp Constr: bad type " <> prettyString t'
    )
checkExp (RecordLit fs loc) = do
  (fs', fs_als) <- mapAndUnzipM checkField fs
  pure (RecordLit fs' loc, Scalar $ Record $ M.fromList fs_als)
  where
    checkField (RecordFieldExplicit name e floc) = do
      (e', e_als) <- checkExp e
      pure (RecordFieldExplicit name e' floc, (unLoc name, e_als))
    checkField (RecordFieldImplicit name t floc) = do
      name_als <- observeVar (locOf floc) (qualName (unLoc name)) $ unInfo t
      pure (RecordFieldImplicit name t floc, (baseName (unLoc name), name_als))

-- Cases that create alias-free values.
checkExp e@(AppExp Range {} _) = noAliases e
checkExp e@IntLit {} = noAliases e
checkExp e@FloatLit {} = noAliases e
checkExp e@Literal {} = noAliases e
checkExp e@StringLit {} = noAliases e
checkExp e@ArrayVal {} = noAliases e
checkExp e@ArrayLit {} = noAliases e
checkExp e@Negate {} = noAliases e
checkExp e@Not {} = noAliases e
checkExp e@Hole {} = noAliases e

-- | Type-check a value definition.  This also infers a new return
-- type that may be fresher than previously.
checkValDef ::
  -- | The declared type of any global, along with the type parameters it is
  -- polymorphic in.  See Note [Parametric results].
  (QualName VName -> Maybe ([TypeParam], StructType)) ->
  (VName, [TypeParam], [Pat ParamType], Exp, ResRetType, Maybe (TypeExp Exp VName), SrcLoc) ->
  ((Exp, ResRetType), [TypeError])
checkValDef globals (fname, tparams, params, body, RetType ext ret, retdecl, loc) = runCheckM globals' (locOf loc) $ do
  fmap fst . bindingParams params $ do
    (body', body_als) <- checkExp body
    checkReturnAlias loc params ret body_als
    -- If the user did not provide an annotation (meaning the return
    -- type is fully inferred), we infer the freshness.  Otherwise,
    -- we go with whatever they wanted.  This lets the user define
    -- nonfresh return types even if the body actually has no
    -- aliases.
    ret' <- case retdecl of
      Just retdecl' -> do
        when (null params && fresh ret) $
          addError retdecl' mempty "A top-level constant cannot be declared fresh."
        pure $ RetType ext ret
      Nothing ->
        pure $
          RetType ext $
            inferReturnFreshness params ret body_als

    pure
      ( (body', ret'),
        body_als -- Don't matter.
      )
  where
    -- Recursion is monomorphic, so a recursive call is at the type parameters
    -- of the definition itself.
    globals' qn
      | qualLeaf qn == fname = Just (tparams, funType params (RetType ext ret))
      | otherwise = globals qn
{-# NOINLINE checkValDef #-}

-- Note [Parametric results]
--
-- Parametricity tells us three things about the result of applying a global
-- function, all read from its *declared* type scheme, which 'envGlobal' looks
-- up: whether the application may have manufactured a value with internal
-- aliasing, whether the result must be fresh because it is the result of an
-- argument that constructs its results freshly, and whether the result may
-- alias a global.
--
-- ## Internal aliasing
--
-- The type system cannot talk about a value that aliases *itself*. For example,
-- a value that is, behind an abstraction boundary, a pair of arrays that are
-- really the same array. Such a value can never be consumed, nor given a fresh
-- type. 'AliasSelf' stands for that possibility. It is not an alias of any
-- variable ('aliasVar' is 'Nothing' for it), so it must never be mistaken for
-- one; ask "might these two values share memory?" through 'overlaps' rather
-- than by comparing alias sets directly. We can think that when 'AliasSelf' is
-- part of an aliasing set then we have "imprecise aliases", and otherwise we
-- have "precise aliaseS" (but do not take these terms literally; aliasing is
-- almost always an over-approximation).
--
-- Parametricity is what tells us whether such a value can have been
-- manufactured by a given function application.
--
-- The crude answer - a value has internal aliasing whenever it is produced by
-- applying a function whose result type is a nonfresh abstract type - is
-- sound but far too coarse.  It refuses
--
--   module pm (M: {type t}) = {
--     def f (x: *M.t) : *M.t = id x
--   }
--
-- because @id@ is instantiated at @M.t -> M.t@. But @id@ manufactures nothing:
-- its declared type @a -> a@ means, by parametricity, that what it returns *is*
-- its argument, whose aliases we know precisely.
--
-- So we perform a more sophisticated reasonong: a function can only manufacture
-- an abstract value if its result mentions an abstract type that is not one of
-- its own type parameters ('manufacturesAbstract'). This is not the same as
-- "the abstract type also occurs in a parameter": a monomorphic @f: M.t -> M.t@
-- inside a module might well be @\_ -> M.mk 5@, so its type tells us nothing
-- (tests/uniqueness/uniqueness-error75.fut). Only genuine polymorphism does.
--
-- Consumption checking sees only instantiated types, so the declared type is
-- looked up when a name is mentioned ('envGlobal', consulted by 'observeVar')
-- and the answer recorded in the type as an 'AliasSelf' on each function
-- component ('noteSelfAliases').  From there ordinary alias propagation carries
-- it: through binding, so @let my_mk = M.mk in my_mk n@ still manufactures;
-- through 'returnType', so partial application does not lose it, and neither
-- does passing the function as an argument, so @n |> M.mk@ manufactures even
-- though @|>@ itself does not.  No arity bookkeeping is needed, because the
-- note means the same thing at every arity: on a function, "applying this may
-- yield an internally-aliased value", and on a value, "this may have internal
-- aliasing".  'returnType' moves between the two readings for free as the
-- result stops being an arrow.
--
-- This is a conservative over-approximation we use whenever we have no better
-- ifnormation available. Hence 'unknownAliases', used for parameters
-- ('selfAliasType') and for any type we build out of thin air, and hence
-- 'closureAliases' keeping the note that a function defined here picked up from
-- its own body. Alias sets are combined by union, and a union of "may" is again
-- a "may"; the join of branches in Note [Locations] drops aliases,
-- but never 'AliasSelf'. In in some sense, 'AliasSelf' behaves a bit like the
-- top element of a lattice.
--
-- ## Freshness
--
-- Futhark does not have "freshness polymorphism", but we can infer it in some
-- cases by parametricity. Consider
--
--   def (|>) 'a '^b (x: a) (f: a -> b) : b = f x
--
-- The result of @|>@ is a type parameter that occurs in exactly one of the
-- parameters, and there only as the result of a function. The only way to
-- obtain a value of a type parameter is to be handed one, and no parameter but
-- @f@ holds any, so the result of @|>@ is necessarily the result of applying
-- @f@ ('resultFromParam'). When @f@ in addition constructs its result freshly -
-- as @copy: t -> *t@ does - so does the application.
--
-- This is a property of the application, not of @|>@ or of its instantiation:
-- @xs |> copy@ is fresh and @xs |> id@ is not, at the very same instantiation.
-- Recall that the instantiation from Terms.hs does not have freshness
-- propagated, so the result in the type is nonfresh, as specified in the type
-- scheme. However, we here update the _instantiated_ type of @|>@ at the
-- application ('parametricFreshness'), which becomes
--
--   (x: []i32) -> (f: []i32 -> *[]i32) -> *[]i32
--
-- From there the ordinary rule for applying a function with a fresh return
-- type does the rest, and later passes get it for free: the monomorphiser keys
-- instances on the type, so @xs |> copy@ and @xs |> id@ become distinct
-- instances, and 'freshenFromInst' in Futhark.Internalise.Monomorphise carries
-- the freshness into the generated definition. Both slots must be marked, not
-- just the result: that definition has body @f x@, which would not justify a
-- fresh result if @f@ were still declared to return a nonfresh one.
--
-- A core restriction is that only an application that supplies every parameter
-- of the function's *type* is refined. A partial application may already have
-- evaluated part of the function's body, and the closure it produces may then
-- hold what that part computed. Consider
--
--   def trap 'a 'b 'c (f: a -> b) (x: a) : c -> b =
--     let r = f x in \(_: c) -> r
--
-- Each call @trap mk_new x u@ computes its own @r@, so its result is fresh. But
-- @k = trap mk_new x@ computes @r@ once, and every call of @k@ returns that
-- same @r@ (tests/higher-order-functions/trap.fut). Read plainly, the result
-- of calling @k@ aliases @k@, which is what makes consuming it safe. The type
-- does not say how much of it a partial application evaluates, so no partial
-- application is refined - an operator section included.
--
-- Which parameter a type variable came from is a fact about the declared type,
-- which the instantiated type does not record, so the applied expression must
-- be a direct mention of a named global. Semantically equal programs are
-- therefore treated differently -
--
--   xs |> copy      -- fresh
--   (|>) xs copy    -- not fresh
--
-- - and the refinement is lost by anything that obscures the head, including
-- parentheses and @let@. This is never *wrong*, only conservative: a spelling
-- we do not recognise yields the plain reading.
--
-- ## Globals
--
-- A use of a global aliases that global, and globals cannot be consumed. When
-- applying a global function returning a nonfresh result, the result thereby
-- aliases the global, and hence cannot be consumed.
--
-- Parametricity keeps polymorphic functions such as @transpose@ precise
-- ('globalAliases'). An edge case is polymorphic "values", which arise solely
-- (I think) for arrays, as an empty array literal can have any element type. To
-- address this, our semantics for polymorphic values is that they are
-- implicitly functions; re-computed on every access to the global. The
-- monomorphiser ensures that by compiling a polymorphic value as a function of
-- @()@, so that each use computes it anew, just as a size-polymorphic value is
-- a function of its sizes. Hence:
--
-- - A component that is not a function, and whose declared type contains one of
--   the global's type parameters, does not alias the global. So @empty 'a :
--   []a@ aliases nothing, while the first component of @pv 'a : ([]i32, []a)@
--   aliases @pv@. An array such as @[](a, i32)@ also counts: it has elements
--   only if it was handed some, and the only way of pairing them with other
--   data, @zip@, constructs its result freshly.
--
-- - A type parameter that is an argument to an abstract type does not count.
--   The module defining @M.t@ may well have @type t 'a = i32@, and so a value
--   of type @M.t a@ or @[](M.t a)@ may be a global.
--
-- - A function aliases the global exactly when some nonfresh component of its
--   (curried) result is not of that kind. So @transpose@, @reverse@ and @|>@
--   alias nothing, and their results alias only their arguments, while the
--   results of @tail : []i32 -> []i32@ alias @tail@ and cannot be consumed. A
--   function has a single alias set, so in a result such as @([]i32, a)@ the
--   second component is also taken to alias the function.
--
-- Size parameters do not count: as far as types can tell, @zeros [n] : [n]i32@
-- might be (a slice of) a global, so it aliases @zeros@.
--
-- This reasoning needs the type parameters, which monomorphisation removes.
-- Hence, monomorphising a Futhark program results in a program that may no
-- longer be correct as far as (this) alias analysis is concerned.

-- Note [Locations]
--
-- Compound types (records and sums) do not have identity. Instead, we track
-- aliasing and consumption at the level of their components ("leaves"). A
-- "path" is a reference to the component of a compound type.
--
-- A location is a variable together with a path ('Location'); every 'Alias'
-- except 'AliasSelf' denotes one. The consumed set holds locations, and a
-- location is dead if a location on the same variable has been consumed whose
-- path is a prefix of its own, or of which its own is a prefix ('deadIn').
-- Consuming @p.a@ kills @p.a@, everything under it, and @p@ itself; consuming
-- @p@ kills all of @p@.
--
-- The payload of a constructor is treated exactly as a tuple, nested under the
-- constructor name: the payload of @#foo xs ys@ has the paths @[foo, 0]@ and
-- @[foo, 1]@.  The components of a sum are therefore separate parts, so
-- @#foo u u@ is self-aliasing just like @(u, u)@, and its payload can be taken
-- apart by pattern matching just like a tuple.
--
-- Using a variable reads all of it: its alias set has a location for each of
-- its leaves, and observing it requires them all to be alive.  Projection
-- happens afterwards.  So to consume one component of a tuple and keep using
-- another, take the tuple apart first:
--
--   let (a, b) = p in let a[0] = 1 in b      -- accepted
--   let a = p.0 in let a[0] = 1 in p.1       -- rejected: p.1 reads p
--
-- Components that may coincide have overlapping alias sets, which is what
-- 'separated' and 'unfreshness' look for.  Building a value preserves this, as
-- each component aliases what it was built from, and so does applying a
-- function or running a loop, as every nonfresh component of the result aliases
-- the same internal name.  The exceptions are an observed parameter, whose
-- components the caller may have made coincide, and a component with
-- 'AliasSelf'.  Neither can be consumed, and the locations of an observed
-- parameter never die.
--
-- The join of the branches of an @if@ or @match@ ('joinBranches') keeps an alias of the combined branch
-- results if it is 'AliasSelf', or if its location and every location in its
-- alias set ('aliasOf') are alive after the branches; the other aliases are
-- consumed.  Filtering by liveness in this way, rather than subtracting the
-- consumed set, is closed under aliasing: if an alias survives, so does
-- everything it aliases.  This is the only place where aliases are dropped, and
-- it may drop the evidence that two components coincide.  Consider
--
--   let (r0, r1) = if c then (u, u) else (let z = u with [0] = 5 in (a, b))
--
-- The else branch consumes @u@, so the filter alone would leave the aliases
-- @({a}, {b})@, claiming that the components of the result are separate, which
-- is false when @c@ holds.  So when the components of some branch's result
-- overlap, the join adds an alias of a fresh name to every component of the
-- combined result.  Being fresh, it is alive after the branches, and as it is
-- consumed along with any component, consuming @r0@ kills @r1@.
--
-- A consumed argument must have separate components ('noSelfAliases'), must not
-- alias anything consumed, and must not overlap the function being applied or
-- any argument evaluated before it ('checkArg').
--
-- A loop is checked as a recursive call whose arguments are what its body
-- returns ('checkLoopResult').  The value returned for a consumed loop
-- parameter is consumed in the next iteration, so it must be fresh in exactly
-- the sense below, with the loop parameters as the parameters.  The diets of
-- the loop parameters are inferred first: a parameter is consumed if the body
-- consumes it or the value returned for a consumed parameter aliases it
-- ('convergeLoopParam').
--
-- A component of a function's result may be fresh exactly when every in-scope
-- location it aliases lies within a consumed part of a parameter, none of its
-- locations occurs in another component, and it is not 'selfAliased'.  The one
-- predicate ('unfreshness') both infers fresh return types and checks declared
-- ones, so declared freshness never exceeds what would be inferred.
