-- | Check that a value definition does not violate any consumption
-- constraints.
module Language.Futhark.TypeChecker.Consumption
  ( checkValDef,
  )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifoldable
import Data.Bifunctor
import Data.DList qualified as DL
import Data.Foldable
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Debug.Trace
import Futhark.Util.Pretty hiding (space)
import Language.Futhark
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Monad (Notes, TypeError (..), withIndexLink)
import Prelude hiding (mod)

type Names = S.Set VName

-- | A variable that is aliased.  Can be still in-scope, or have gone
-- out of scope and be free.  In the latter case, it behaves more like
-- an equivalence class.  See uniqueness-error18.fut for an example of
-- why this is necessary.
data Alias
  = AliasBound {aliasVar :: VName}
  | AliasFree {aliasVar :: VName}
  deriving (Eq, Ord, Show)

instance Pretty Alias where
  pretty (AliasBound v) = prettyName v
  pretty (AliasFree v) = "~" <> prettyName v

instance Pretty (S.Set Alias) where
  pretty = braces . commasep . map pretty . S.toList

-- | The set of in-scope variables that are being aliased.
boundAliases :: Aliases -> S.Set VName
boundAliases = S.map aliasVar . S.filter bound
  where
    bound AliasBound {} = True
    bound AliasFree {} = False

-- | Aliases for a type, which is a set of the variables that are
-- aliased.
type Aliases = S.Set Alias

type TypeAliases = TypeBase Size Aliases

-- | @t \`setAliases\` als@ returns @t@, but with @als@ substituted for
-- any already present aliases.
setAliases :: TypeBase dim asf -> ast -> TypeBase dim ast
setAliases t = addAliases t . const

-- | @t \`addAliases\` f@ returns @t@, but with any already present
-- aliases replaced by @f@ applied to that aliases.
addAliases ::
  TypeBase dim asf ->
  (asf -> ast) ->
  TypeBase dim ast
addAliases = flip second

aliases :: TypeAliases -> Aliases
aliases = bifoldMap (const mempty) id

setFieldAliases :: TypeAliases -> [Name] -> TypeAliases -> TypeAliases
setFieldAliases ve_als (x : xs) (Scalar (Record fs)) =
  Scalar $ Record $ M.adjust (setFieldAliases ve_als xs) x fs
setFieldAliases ve_als _ _ = ve_als

data Entry a
  = Consumable {entryAliases :: a}
  | Nonconsumable {entryAliases :: a}
  deriving (Eq, Ord, Show)

instance Functor Entry where
  fmap f (Consumable als) = Consumable $ f als
  fmap f (Nonconsumable als) = Nonconsumable $ f als

newtype CheckEnv = CheckEnv
  { envVtable :: M.Map VName (Entry TypeAliases)
  }

-- | A description of where an artificial compiler-generated
-- intermediate name came from.
data NameReason
  = -- | Name is the result of a function application.
    NameAppRes (Maybe (QualName VName)) SrcLoc
  | NameLoopRes SrcLoc

nameReason :: SrcLoc -> NameReason -> Doc a
nameReason loc (NameAppRes Nothing apploc) =
  "result of application at" <+> pretty (locStrRel loc apploc)
nameReason loc (NameAppRes fname apploc) =
  "result of applying"
    <+> dquotes (pretty fname)
    <+> parens ("at" <+> pretty (locStrRel loc apploc))
nameReason loc (NameLoopRes apploc) =
  "result of loop at" <+> pretty (locStrRel loc apploc)

type Consumed = M.Map VName Loc

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

runCheckM :: CheckM a -> (a, [TypeError])
runCheckM (CheckM m) =
  let (a, s) = runState (runReaderT m env) initial_state
   in (a, DL.toList (stateErrors s))
  where
    env =
      CheckEnv
        { envVtable = mempty
        }
    initial_state =
      CheckState
        { stateConsumed = mempty,
          stateErrors = mempty,
          stateNames = mempty,
          stateCounter = 0
        }

describeVar :: Loc -> VName -> CheckM (Doc a)
describeVar loc v =
  gets $
    maybe ("variable" <+> dquotes (prettyName v)) (nameReason (srclocOf loc))
      . M.lookup v
      . stateNames

noConsumable :: CheckM a -> CheckM a
noConsumable = local $ \env -> env {envVtable = M.map f $ envVtable env}
  where
    f = Nonconsumable . entryAliases

addError :: Located loc => loc -> Notes -> Doc () -> CheckM ()
addError loc notes e = modify $ \s ->
  s {stateErrors = DL.snoc (stateErrors s) (TypeError (locOf loc) notes e)}

incCounter :: CheckM Int
incCounter =
  state $ \s -> (stateCounter s, s {stateCounter = stateCounter s + 1})

returnAliased :: Name -> SrcLoc -> CheckM ()
returnAliased name loc =
  addError loc mempty . withIndexLink "return-aliased" $
    "Unique-typed return value is aliased to"
      <+> dquotes (prettyName name) <> ", which is not consumable."

uniqueReturnAliased :: SrcLoc -> CheckM ()
uniqueReturnAliased loc =
  addError loc mempty . withIndexLink "unique-return-aliased" $
    "A unique-typed component of the return value is aliased to some other component."

checkReturnAlias :: SrcLoc -> [Pat ParamType] -> ResType -> TypeAliases -> CheckM ()
checkReturnAlias loc params rettp =
  foldM_ (checkReturnAlias' params) S.empty . returnAliases rettp
  where
    checkReturnAlias' params' seen (Unique, names) = do
      when (any (`S.member` S.map snd seen) $ S.toList names) $
        uniqueReturnAliased loc
      notAliasesParam params' names
      pure $ seen `S.union` tag Unique names
    checkReturnAlias' _ seen (Nonunique, names) = do
      when (any (`S.member` seen) $ S.toList $ tag Unique names) $
        uniqueReturnAliased loc
      pure $ seen `S.union` tag Nonunique names

    notAliasesParam params' names =
      forM_ params' $ \p ->
        let consumedNonunique (v, t) =
              not (consumableParamType t) && (v `S.member` names)
         in case find consumedNonunique $ patternMap p of
              Just (v, _) ->
                returnAliased (baseName v) loc
              Nothing ->
                pure ()

    tag u = S.map (u,)

    returnAliases (Scalar (Record ets1)) (Scalar (Record ets2)) =
      concat $ M.elems $ M.intersectionWith returnAliases ets1 ets2
    returnAliases expected got =
      [(uniqueness expected, S.map aliasVar $ aliases got)]

    consumableParamType (Array u _ _) = u == Consume
    consumableParamType (Scalar Prim {}) = True
    consumableParamType (Scalar (TypeVar u _ _)) = u == Consume
    consumableParamType (Scalar (Record fs)) = all consumableParamType fs
    consumableParamType (Scalar (Sum fs)) = all (all consumableParamType) fs
    consumableParamType (Scalar Arrow {}) = False

unscope :: Names -> Aliases -> Aliases
unscope bound = S.map f
  where
    f (AliasFree v) = AliasFree v
    f (AliasBound v) = if v `S.member` bound then AliasFree v else AliasBound v

-- | Figure out the aliases of each bound name in a pattern.
matchPat :: Pat t -> TypeAliases -> DL.DList (VName, (t, TypeAliases))
matchPat (PatParens p _) t = matchPat p t
matchPat (TuplePat ps _) t
  | Just ts <- isTupleRecord t = mconcat $ zipWith matchPat ps ts
matchPat (RecordPat fs1 _) (Scalar (Record fs2)) =
  mconcat $
    zipWith
      matchPat
      (map snd (sortFields (M.fromList fs1)))
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
            foldr (uncurry M.insert) (envVtable env) (fmap f (matchPat p t))
        }
      where
        f (v, (_, als)) = (v, Consumable $ second (S.insert (AliasBound v)) als)

bindingParam :: Pat ParamType -> CheckM (a, TypeAliases) -> CheckM (a, TypeAliases)
bindingParam p m = do
  mapM_ (noConsumable . bitraverse_ checkExp pure) p
  second (second (unscope (patNames p))) <$> local bind m
  where
    bind env =
      env
        { envVtable =
            foldr (uncurry M.insert) (envVtable env) (fmap f (patternMap p))
        }
    f (v, t)
      | diet t == Consume = (v, Consumable $ t `setAliases` S.singleton (AliasBound v))
      | otherwise = (v, Nonconsumable $ t `setAliases` S.singleton (AliasBound v))

bindingIdent :: Diet -> Ident StructType -> CheckM (a, TypeAliases) -> CheckM (a, TypeAliases)
bindingIdent d (Ident v (Info t) _) =
  fmap (second (second (unscope (S.singleton v)))) . local bind
  where
    bind env = env {envVtable = M.insert v t' (envVtable env)}
    d' = case d of
      Consume -> Consumable
      Observe -> Nonconsumable
    t' = d' $ t `setAliases` S.singleton (AliasBound v)

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

binding :: VName -> TypeAliases -> CheckM a -> CheckM a
binding v t = local $ \env -> env {envVtable = M.insert v (Consumable t) (envVtable env)}

addSelfAliases :: VName -> TypeAliases -> TypeAliases
addSelfAliases v (Array als shape t) = Array (S.insert (AliasBound v) als) shape t
addSelfAliases v (Scalar st) = Scalar $ addSelfAliases' st
  where
    addSelfAliases' (TypeVar als tn args) = TypeVar (S.insert (AliasBound v) als) tn args
    addSelfAliases' (Record fs) = Record $ fmap (addSelfAliases v) fs
    addSelfAliases' (Sum fs) = Sum $ fmap (map (addSelfAliases v)) fs
    addSelfAliases' t@Arrow {} = t
    addSelfAliases' t@Prim {} = t

lookupAliases :: VName -> StructType -> CheckM Aliases
lookupAliases v t =
  asks $
    aliases
      . addSelfAliases v
      . maybe (second (const mempty) t) entryAliases
      . M.lookup v
      . envVtable

checkIfConsumed :: Loc -> Aliases -> CheckM ()
checkIfConsumed rloc als = do
  cons <- gets stateConsumed
  let bad v = fmap (v,) $ v `M.lookup` cons
  forM_ (mapMaybe (bad . aliasVar) $ S.toList als) $ \(v, wloc) -> do
    v' <- describeVar rloc v
    addError rloc mempty . withIndexLink "use-after-consume" $
      "Using"
        <+> v' <> ", but this was consumed at"
        <+> pretty (locStrRel rloc wloc) <> ".  (Possibly through aliases.)"

observeAliases :: Loc -> Aliases -> CheckM ()
observeAliases = checkIfConsumed

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
      checkIfConsumable (AliasBound v)
        | isBad v = do
            v' <- describeVar loc v
            addError loc mempty . withIndexLink "not-consumable" $
              "Would consume" <+> v' <> ", which is not consumable."
      checkIfConsumable _ = pure ()
  mapM_ checkIfConsumable $ S.toList als
  checkIfConsumed loc als
  consumed als'
  where
    als' = M.fromList $ map ((,loc) . aliasVar) $ S.toList als

consume :: Loc -> VName -> StructType -> CheckM ()
consume loc v t =
  consumeAliases loc =<< lookupAliases v t

-- | Observe the given name here and return its aliases.
observeVar :: Loc -> VName -> StructType -> CheckM TypeAliases
observeVar loc v t = do
  als <-
    asks $
      maybe (addSelfAliases v $ second (const mempty) t) entryAliases
        . M.lookup v
        . envVtable
  observeAliases loc (aliases als)
  pure als

-- Capture any newly consumed variables that occur during the provided action.
contain :: CheckM a -> CheckM (a, Consumed)
contain m = do
  prev_cons <- gets stateConsumed
  x <- m
  new_cons <- gets $ (`M.difference` prev_cons) . stateConsumed
  modify $ \s -> s {stateConsumed = prev_cons}
  pure (x, new_cons)

maskAliases :: Aliases -> Diet -> Aliases
maskAliases _ Consume = mempty
maskAliases als Observe = als

-- | The two types are assumed to be approximately structurally equal,
-- but not necessarily regarding sizes.  Combines aliases and prefers
-- other information from first argument.
combineAliases :: TypeAliases -> TypeAliases -> TypeAliases
combineAliases (Array als1 et1 shape1) t2 =
  Array (als1 <> aliases t2) et1 shape1
combineAliases (Scalar (TypeVar als1 tv1 targs1)) t2 =
  Scalar $ TypeVar (als1 <> aliases t2) tv1 targs1
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

-- An alias inhibits uniqueness if it is used in disjoint values.
aliasesMultipleTimes :: TypeAliases -> Names
aliasesMultipleTimes = S.fromList . map fst . filter ((> 1) . snd) . M.toList . delve
  where
    delve (Scalar (Record fs)) =
      foldl' (M.unionWith (+)) mempty $ map delve $ M.elems fs
    delve t =
      M.fromList $ zip (map aliasVar $ S.toList (aliases t)) $ repeat (1 :: Int)

consumingParams :: [Pat ParamType] -> Names
consumingParams =
  S.fromList . map fst . filter ((== Consume) . diet . snd) . foldMap patternMap

arrayAliases :: TypeAliases -> Aliases
arrayAliases (Array als _ _) = als
arrayAliases (Scalar Prim {}) = mempty
arrayAliases (Scalar (Record fs)) = foldMap arrayAliases fs
arrayAliases (Scalar (TypeVar als _ _)) = als
arrayAliases (Scalar Arrow {}) = mempty
arrayAliases (Scalar (Sum fs)) =
  mconcat $ concatMap (map arrayAliases) $ M.elems fs

overlapCheck :: (Pretty src, Pretty ve) => Loc -> (src, TypeAliases) -> (ve, TypeAliases) -> CheckM ()
overlapCheck loc (src, src_als) (ve, ve_als) =
  when (any (`S.member` aliases src_als) (aliases ve_als)) $
    addError loc mempty $
      "Source array for in-place update"
        </> indent 2 (pretty src)
        </> "might alias update value"
        </> indent 2 (pretty ve)
        </> "Hint: use"
        <+> dquotes "copy"
        <+> "to remove aliases from the value."

inferReturnUniqueness :: [Pat ParamType] -> ResType -> TypeAliases -> ResType
inferReturnUniqueness params ret ret_als = delve ret ret_als
  where
    bound = foldMap patNames params
    forbidden = aliasesMultipleTimes ret_als
    consumings = consumingParams params
    delve (Scalar (Record fs1)) (Scalar (Record fs2)) =
      Scalar $ Record $ M.intersectionWith delve fs1 fs2
    delve (Scalar (Sum cs1)) (Scalar (Sum cs2)) =
      Scalar $ Sum $ M.intersectionWith (zipWith delve) cs1 cs2
    delve t t_als
      | all (`S.member` consumings) $
          S.map aliasVar (arrayAliases t_als) `S.intersection` bound,
        not $ any ((`S.member` forbidden) . aliasVar) (aliases t_als) =
          t `setUniqueness` Unique
      | otherwise =
          t `setUniqueness` Nonunique

-- | @returnType appres ret_type arg_diet arg_type@ gives result of applying
-- an argument the given types to a function with the given return
-- type, consuming the argument with the given diet.
returnType :: Aliases -> ResType -> Diet -> TypeAliases -> TypeAliases
returnType _ (Array Unique et shape) _ _ =
  Array mempty et shape
returnType appres (Array Nonunique et shape) d arg =
  Array (appres <> arg_als) et shape
  where
    arg_als = maskAliases (aliases arg) d
returnType appres (Scalar (Record fs)) d arg =
  Scalar $ Record $ fmap (\et -> returnType appres et d arg) fs
returnType _ (Scalar (Prim t)) _ _ =
  Scalar $ Prim t
returnType _ (Scalar (TypeVar Unique t targs)) _ _ =
  Scalar $ TypeVar mempty t targs
returnType appres (Scalar (TypeVar Nonunique t targs)) d arg =
  Scalar $ TypeVar (appres <> arg_als) t targs
  where
    arg_als = maskAliases (aliases arg) d
returnType _ (Scalar (Arrow _ v pd t1 (RetType dims t2))) d arg =
  Scalar $ Arrow als v pd (t1 `setAliases` mempty) $ RetType dims t2
  where
    als = maskAliases (aliases arg) d
returnType appres (Scalar (Sum cs)) d arg =
  Scalar $ Sum $ (fmap . fmap) (\et -> returnType appres et d arg) cs

checkSubExps :: ASTMappable e => e -> CheckM e
checkSubExps = astMap identityMapper {mapOnExp = fmap fst . checkExp}

noAliases :: Exp -> CheckM (Exp, TypeAliases)
noAliases e = do
  e' <- checkSubExps e
  pure (e', second (const mempty) (typeOf e))

aliasParts :: TypeAliases -> [Aliases]
aliasParts (Scalar (Record ts)) = foldMap aliasParts $ M.elems ts
aliasParts t = [aliases t]

noSelfAliases :: Loc -> TypeAliases -> CheckM ()
noSelfAliases loc = foldM_ check mempty . aliasParts
  where
    check seen als = do
      when (any (`S.member` seen) als) $
        addError loc mempty . withIndexLink "self-aliases-arg" $
          "Argument passed for consuming parameter is self-aliased."
      pure $ als <> seen

consumeAsNeeded :: Loc -> ParamType -> TypeAliases -> CheckM ()
consumeAsNeeded loc pt t =
  when (diet pt == Consume) $ consumeAliases loc $ aliases t

checkArg :: ParamType -> Exp -> CheckM (Exp, TypeAliases)
checkArg p_t e = do
  ((e', e_als), e_cons) <- contain $ checkExp e
  consumed e_cons
  let e_t = typeOf e'
  when (e_cons /= mempty && not (orderZero e_t)) $
    addError (locOf e) mempty $
      "Argument of functional type"
        </> indent 2 (pretty e_t)
        </> "contains consumption, which is not allowed."
  when (diet p_t == Consume) $ do
    noSelfAliases (locOf e) e_als
    consumeAsNeeded (locOf e) p_t e_als
  pure (e', e_als)

applyArg :: TypeAliases -> TypeAliases -> TypeAliases
applyArg (Scalar (Arrow closure_als _ d _ (RetType _ rettype))) arg_als =
  returnType closure_als rettype d arg_als
applyArg t _ = error $ "applyArg: " <> show t

-- Loops are tricky because we want to infer the uniqueness of their
-- parameters.  This is pretty unusual: we do not do this for ordinary
-- functions.
type Loop = (Pat ParamType, Exp, LoopFormBase Info VName, Exp)

-- | Mark bindings of consumed names as Consume.
updateParamDiet :: Names -> Pat ParamType -> Pat ParamType
updateParamDiet cons = recurse
  where
    recurse (Wildcard (Info t) wloc) =
      Wildcard (Info $ t `setUniqueness` Observe) wloc
    recurse (PatParens p ploc) =
      PatParens (recurse p) ploc
    recurse (PatAttr attr p ploc) =
      PatAttr attr (recurse p) ploc
    recurse (Id name (Info t) iloc)
      | name `S.member` cons =
          let t' = t `setUniqueness` Consume
           in Id name (Info t') iloc
      | otherwise =
          let t' = t `setUniqueness` Observe
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

convergeLoopParam :: Loc -> Pat ParamType -> Names -> TypeAliases -> CheckM (Pat ParamType)
convergeLoopParam loop_loc param body_cons body_als = do
  let -- Make the pattern Consume where needed.
      param' = updateParamDiet (patNames param `S.intersection` body_cons) param

  -- Check that the new values of consumed merge parameters do not
  -- alias something bound outside the loop, AND that anything
  -- returned for a unique merge parameter does not alias anything
  -- else returned.
  let checkMergeReturn (Id pat_v (Info pat_v_t) patloc) t = do
        let free_als = boundAliases (aliases t) `S.difference` patNames param
        when (diet pat_v_t == Consume) $ forM_ free_als $ \v ->
          lift . addError loop_loc mempty $
            "Return value for consuming loop parameter"
              <+> dquotes (prettyName pat_v)
              <+> "aliases"
              <+> dquotes (prettyName v) <> "."
        (cons, obs) <- get
        unless (S.null $ aliases t `S.intersection` cons) $
          lift . addError loop_loc mempty $
            "Return value for loop parameter"
              <+> dquotes (prettyName pat_v)
              <+> "aliases other consumed loop parameter."
        when
          ( diet pat_v_t == Consume
              && not (S.null (aliases t `S.intersection` (cons <> obs)))
          )
          $ lift . addError loop_loc mempty
          $ "Return value for consuming loop parameter"
            <+> dquotes (prettyName pat_v)
            <+> "aliases previously returned value."
        if diet pat_v_t == Consume
          then put (cons <> aliases t, obs)
          else put (cons, obs <> aliases t)

        pure $ Id pat_v (Info pat_v_t) patloc
      checkMergeReturn (Wildcard (Info pat_v_t) patloc) _ =
        pure $ Wildcard (Info pat_v_t) patloc
      checkMergeReturn (PatParens p _) t =
        checkMergeReturn p t
      checkMergeReturn (PatAscription p _ _) t =
        checkMergeReturn p t
      checkMergeReturn (RecordPat pfs patloc) (Scalar (Record tfs)) =
        RecordPat . M.toList <$> sequence pfs' <*> pure patloc
        where
          pfs' = M.intersectionWith checkMergeReturn (M.fromList pfs) tfs
      checkMergeReturn (TuplePat pats patloc) t
        | Just ts <- isTupleRecord t =
            TuplePat <$> zipWithM checkMergeReturn pats ts <*> pure patloc
      checkMergeReturn p _ =
        pure p

  (param'', (param_cons, _)) <-
    runStateT (checkMergeReturn param' body_als) (mempty, mempty)

  let body_cons' = body_cons <> S.map aliasVar param_cons
  traceM $
    unlines
      [ "convergeLoopParam",
        prettyString param,
        show body_cons,
        prettyString body_als
      ]
  if body_cons' == body_cons && patternType param'' == patternType param
    then pure param'
    else convergeLoopParam loop_loc param'' body_cons' body_als

checkLoop :: Loc -> Loop -> CheckM (Loop, TypeAliases)
checkLoop loop_loc (param, arg, form, body) = do
  form' <- checkSubExps form
  -- We pretend that every part of the loop parameter has a consuming
  -- diet, as we need to allow consumption in the body, which we then
  -- use to infer the proper diet of the parameter.
  ((body', body_cons), body_als) <-
    noConsumable
      . bindingParam (fmap (second (const Consume)) param)
      . bindingLoopForm form'
      $ do
        ((body', body_als), body_cons) <- contain $ checkExp body
        pure ((body', body_cons), body_als)
  param' <- convergeLoopParam loop_loc param (M.keysSet body_cons) body_als

  let param_t = patternType param'
  traceM $ unlines ["checkArg", prettyString param_t, prettyString arg]
  (arg', arg_als) <- checkArg param_t arg
  v <- VName "internal_loop_result" <$> incCounter
  modify $ \s -> s {stateNames = M.insert v (NameLoopRes (srclocOf loop_loc)) $ stateNames s}
  let loopt =
        funType [param'] (RetType [] $ paramToRes param_t)
          `setAliases` S.singleton (AliasFree v)
  pure
    ( (param', arg', form', body'),
      applyArg loopt arg_als `combineAliases` body_als
    )

checkExp :: Exp -> CheckM (Exp, TypeAliases)
-- First we have the complicated cases.

--
checkExp (AppExp (Apply f args loc) appres) = do
  -- Note Futhark uses right-to-left evaluation of applications.
  (args', args_als) <- NE.unzip . NE.reverse <$> traverse checkArg' (NE.reverse args)
  (f', f_als) <- checkExp f
  v <- VName "internal_app_result" <$> incCounter
  modify $ \s -> s {stateNames = M.insert v (NameAppRes (fname f) loc) $ stateNames s}
  let res_als = foldl applyArg (second (S.insert (AliasFree v)) f_als) args_als
  pure (AppExp (Apply f' args' loc) appres, res_als)
  where
    fname (Var v _ _) = Just v
    fname (AppExp (Apply e _ _) _) = fname e
    fname _ = Nothing
    checkArg' (Info (d, p), e) = do
      (e', e_als) <- checkArg (second (const d) (typeOf e)) e
      pure ((Info (d, p), e'), e_als)
--
checkExp (AppExp (LetPat sizes p e body loc) appres) = do
  (e', e_als) <- checkExp e
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
  let all_cons = te_cons <> fe_cons
      notConsumed = not . (`M.member` all_cons) . aliasVar
      comb_als = second (S.filter notConsumed) $ te_als `combineAliases` fe_als
  consumed all_cons
  pure
    ( AppExp (If cond' te' fe' loc) appres,
      appResType (unInfo appres) `setAliases` mempty `combineAliases` comb_als
    )

--
checkExp (AppExp (Match cond cs loc) appres) = do
  (cond', cond_als) <- checkExp cond
  ((cs', cs_als), cs_cons) <- first NE.unzip . NE.unzip <$> mapM (checkCase cond_als) cs
  let all_cons = fold cs_cons
      notConsumed = not . (`M.member` all_cons) . aliasVar
      comb_als = second (S.filter notConsumed) $ foldl1 combineAliases cs_als
  consumed all_cons
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
checkExp (AppExp (LetFun fname (typarams, params, te, Info (RetType ext ret), funbody) letbody loc) appres) = do
  (funbody', funbody_als) <- bindingParams params $ checkExp funbody
  checkReturnAlias loc params ret funbody_als
  checkGlobalAliases loc params funbody_als
  let ret' = inferReturnUniqueness params ret funbody_als
      ftype =
        funType params (RetType ext ret')
          `setAliases` S.map AliasBound (fvVars (freeInExp funbody))
  (letbody', letbody_als) <- binding fname ftype $ checkExp letbody
  pure
    ( AppExp (LetFun fname (typarams, params, te, Info (RetType ext ret), funbody') letbody' loc) appres,
      letbody_als
    )

--
checkExp (AppExp (BinOp (op, oploc) opt (x, xp) (y, yp) loc) appres) = do
  op_als <- observeVar (locOf oploc) (qualLeaf op) (unInfo opt)
  let at1 : at2 : _ = fst $ unfoldFunType op_als
  (x', x_als) <- checkArg at1 x
  (y', y_als) <- checkArg at2 y
  pure
    ( AppExp (BinOp (op, oploc) opt (x', xp) (y', yp) loc) appres,
      foldl applyArg op_als [x_als, y_als]
    )

--
checkExp e@(Lambda params body te (Info (RetType ext ret)) loc) = do
  (body', body_als) <- bindingParams params $ checkExp body
  checkReturnAlias loc params ret body_als
  checkGlobalAliases loc params body_als
  vtable <- asks envVtable
  let free_bound = S.filter (`M.member` vtable) $ fvVars (freeInExp e)
      ret' = inferReturnUniqueness params ret body_als
      als = aliases body_als <> S.map AliasBound free_bound
  pure
    ( Lambda params body' te (Info (RetType ext ret')) loc,
      funType params (RetType ext ret') `setAliases` als
    )

--
checkExp (AppExp (LetWith dst src slice ve body loc) appres) = do
  src_als <- observeVar (locOf dst) (identName src) (unInfo $ identType src)
  slice' <- checkSubExps slice
  (ve', ve_als) <- checkExp ve
  consume (locOf src) (identName src) (unInfo (identType src))
  overlapCheck (locOf ve) (src, src_als) (ve', ve_als)
  (body', body_als) <- bindingIdent Consume dst $ checkExp body
  pure (AppExp (LetWith dst src slice' ve' body' loc) appres, body_als)

--
checkExp (Update src slice ve loc) = do
  slice' <- checkSubExps slice
  (src', src_als) <- checkExp src
  (ve', ve_als) <- checkExp ve
  overlapCheck (locOf ve) (src', src_als) (ve', ve_als)
  consumeAliases (locOf loc) $ aliases src_als
  pure (Update src' slice' ve' loc, second (const mempty) src_als)

-- Cases that simply propagate aliases directly.
checkExp (Var v (Info t) loc) = do
  als <- observeVar (locOf loc) (qualLeaf v) t
  observeAliases (locOf loc) (aliases als)
  pure (Var v (Info t) loc, als)
checkExp (OpSection v (Info t) loc) = do
  als <- observeVar (locOf loc) (qualLeaf v) t
  observeAliases (locOf loc) (aliases als)
  pure (OpSection v (Info t) loc, als)
checkExp (OpSectionLeft op ftype arg arginfo retinfo loc) = do
  let (_, Info (pn, pt2)) = arginfo
      (Info ret, _) = retinfo
  als <- observeVar (locOf loc) (qualLeaf op) (unInfo ftype)
  (arg', arg_als) <- checkExp arg
  pure
    ( OpSectionLeft op ftype arg' arginfo retinfo loc,
      Scalar $ Arrow (aliases arg_als <> aliases als) pn (diet pt2) (toStruct pt2) ret
    )
checkExp (OpSectionRight op ftype arg arginfo retinfo loc) = do
  let (Info (pn, pt2), _) = arginfo
      Info ret = retinfo
  als <- observeVar (locOf loc) (qualLeaf op) (unInfo ftype)
  (arg', arg_als) <- checkExp arg
  pure
    ( OpSectionRight op ftype arg' arginfo retinfo loc,
      Scalar $ Arrow (aliases arg_als <> aliases als) pn (diet pt2) (toStruct pt2) ret
    )
checkExp (IndexSection slice t loc) = do
  slice' <- checkSubExps slice
  pure (IndexSection slice' t loc, unInfo t `setAliases` mempty)
checkExp (ProjectSection fs t loc) = do
  pure (ProjectSection fs t loc, unInfo t `setAliases` mempty)
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
  pure (Project name e' t loc, e_als)
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
checkExp (RecordUpdate src fields ve t loc) = do
  (src', src_als) <- checkExp src
  (ve', ve_als) <- checkExp ve
  pure
    ( RecordUpdate src' fields ve' t loc,
      setFieldAliases ve_als fields src_als
    )
checkExp (RecordLit fs loc) = do
  (fs', fs_als) <- mapAndUnzipM checkField fs
  pure (RecordLit fs' loc, Scalar $ Record $ M.fromList fs_als)
  where
    checkField (RecordFieldExplicit name e floc) = do
      (e', e_als) <- checkExp e
      pure (RecordFieldExplicit name e' floc, (name, e_als))
    checkField (RecordFieldImplicit name t floc) = do
      name_als <- observeVar (locOf floc) name $ unInfo t
      pure (RecordFieldImplicit name t floc, (baseName name, name_als))

-- Cases that create alias-free values.
checkExp e@(AppExp Range {} _) = noAliases e
checkExp e@IntLit {} = noAliases e
checkExp e@FloatLit {} = noAliases e
checkExp e@Literal {} = noAliases e
checkExp e@StringLit {} = noAliases e
checkExp e@ArrayLit {} = noAliases e
checkExp e@Negate {} = noAliases e
checkExp e@Not {} = noAliases e
checkExp e@Hole {} = noAliases e
checkExp (AppExp (DoLoop sparams pat args form body loc) appres) = do
  ((pat', args', form', body'), als) <- checkLoop (locOf loc) (pat, args, form, body)
  pure
    ( AppExp (DoLoop sparams pat' args' form' body' loc) appres,
      als
    )

checkGlobalAliases :: SrcLoc -> [Pat ParamType] -> TypeAliases -> CheckM ()
checkGlobalAliases loc params body_t = do
  vtable <- asks envVtable
  let global = flip M.notMember vtable
  unless (null params) $ forM_ (boundAliases $ arrayAliases body_t) $ \v ->
    when (global v) . addError loc mempty . withIndexLink "alias-free-variable" $
      "Function result aliases the free variable "
        <> dquotes (prettyName v)
        <> "."
        </> "Use"
        <+> dquotes "copy"
        <+> "to break the aliasing."

-- | Type-check a value definition.  This also infers a new return
-- type that may be more unique than previously.
checkValDef ::
  (VName, [Pat ParamType], Exp, ResRetType, SrcLoc) ->
  ((Exp, ResRetType), [TypeError])
checkValDef (_fname, params, body, RetType ext ret, loc) = runCheckM $ do
  (body', body_als) <- bindingParams params $ checkExp body
  checkReturnAlias loc params ret body_als
  checkGlobalAliases loc params body_als
  when (null params && unique ret) $
    addError loc mempty "A top-level constant cannot have a unique type."
  pure (body', RetType ext $ inferReturnUniqueness params ret body_als)
{-# NOINLINE checkValDef #-}
