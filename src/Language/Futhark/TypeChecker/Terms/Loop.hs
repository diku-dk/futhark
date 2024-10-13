-- | Type inference of @loop@.  This is complicated because of the
-- uniqueness and size inference, so the implementation is separate
-- from the main type checker.
module Language.Futhark.TypeChecker.Terms.Loop
  ( UncheckedLoop,
    CheckedLoop,
    checkLoop,
  )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Bitraversable
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.Util (nubOrd)
import Futhark.Util.Pretty hiding (group, space)
import Language.Futhark
import Language.Futhark.TypeChecker.Monad hiding (BoundV)
import Language.Futhark.TypeChecker.Terms.Monad
import Language.Futhark.TypeChecker.Terms.Pat
import Language.Futhark.TypeChecker.Types
import Language.Futhark.TypeChecker.Unify
import Prelude hiding (mod)

-- | Retrieve an oracle that can be used to decide whether two are in
-- the same equivalence class (i.e. have been unified).  This is an
-- exotic operation.
getAreSame :: (MonadUnify m) => m (VName -> VName -> Bool)
getAreSame = check <$> getConstraints
  where
    check constraints x y =
      case (M.lookup x constraints, M.lookup y constraints) of
        (Just (_, Size (Just (Var x' _ _)) _), _) ->
          check constraints (qualLeaf x') y
        (_, Just (_, Size (Just (Var y' _ _)) _)) ->
          check constraints x (qualLeaf y')
        _ ->
          x == y

-- | Replace specified sizes with distinct fresh size variables.
someDimsFreshInType ::
  SrcLoc ->
  Name ->
  [VName] ->
  TypeBase Size als ->
  TermTypeM (TypeBase Size als)
someDimsFreshInType loc desc fresh t = do
  areSameSize <- getAreSame
  let freshen v = any (areSameSize v) fresh
  bitraverse (onDim freshen) pure t
  where
    onDim freshen (Var d _ _)
      | freshen $ qualLeaf d = do
          v <- newFlexibleDim (mkUsage' loc) desc
          pure $ sizeFromName (qualName v) loc
    onDim _ d = pure d

-- | Replace the specified sizes with fresh size variables of the
-- specified ridigity.  Returns the new fresh size variables.
freshDimsInType ::
  Usage ->
  Rigidity ->
  Name ->
  [VName] ->
  TypeBase Size u ->
  TermTypeM (TypeBase Size u, [VName])
freshDimsInType usage r desc fresh t = do
  areSameSize <- getAreSame
  second (map snd) <$> runStateT (bitraverse (onDim areSameSize) pure t) mempty
  where
    onDim areSameSize (Var (QualName _ d) _ _)
      | any (areSameSize d) fresh = do
          prev_subst <- gets $ L.find (areSameSize d . fst)
          case prev_subst of
            Just (_, d') -> pure $ sizeFromName (qualName d') $ srclocOf usage
            Nothing -> do
              v <- lift $ newDimVar usage r desc
              modify ((d, v) :)
              pure $ sizeFromName (qualName v) $ srclocOf usage
    onDim _ d = pure d

data ArgSource = Initial | BodyResult

wellTypedLoopArg :: ArgSource -> [VName] -> Pat ParamType -> Exp -> TermTypeM ()
wellTypedLoopArg src sparams pat arg = do
  (merge_t, _) <-
    freshDimsInType (mkUsage arg desc) Nonrigid "loop" sparams $
      toStruct (patternType pat)
  arg_t <- toStruct <$> expTypeFully arg
  onFailure (checking merge_t arg_t) $
    unify (mkUsage arg desc) merge_t arg_t
  where
    (checking, desc) =
      case src of
        Initial -> (CheckingLoopInitial, "matching initial loop values to pattern")
        BodyResult -> (CheckingLoopBody, "matching loop body to pattern")

-- | An un-checked loop.
type UncheckedLoop =
  (PatBase NoInfo VName ParamType, ExpBase NoInfo VName, LoopFormBase NoInfo VName, ExpBase NoInfo VName)

-- | A loop that has been type-checked.
type CheckedLoop =
  ([VName], Pat ParamType, Exp, LoopFormBase Info VName, Exp)

checkForImpossible :: Loc -> S.Set VName -> ParamType -> TermTypeM ()
checkForImpossible loc known_before pat_t = do
  cs <- getConstraints
  let bad v = do
        guard $ v `S.notMember` known_before
        (_, UnknownSize v_loc _) <- M.lookup v cs
        Just . typeError (srclocOf loc) mempty $
          "Inferred type for loop parameter is"
            </> indent 2 (pretty pat_t)
            </> "but"
            <+> dquotes (prettyName v)
            <+> "is an existential size created inside the loop body at"
            <+> pretty (locStrRel loc v_loc)
              <> "."
  case mapMaybe bad $ S.toList $ fvVars $ freeInType pat_t of
    problem : _ -> problem
    [] -> pure ()

-- | Type-check a @loop@ expression, passing in a function for
-- type-checking subexpressions.
checkLoop ::
  (ExpBase NoInfo VName -> TermTypeM Exp) ->
  UncheckedLoop ->
  SrcLoc ->
  TermTypeM (CheckedLoop, AppRes)
checkLoop checkExp (mergepat, mergeexp, form, loopbody) loc = do
  mergeexp' <- checkExp mergeexp
  known_before <- M.keysSet <$> getConstraints
  zeroOrderType
    (mkUsage mergeexp "use as loop variable")
    "type used as loop variable"
    . toStruct
    =<< expTypeFully mergeexp'

  -- The handling of dimension sizes is a bit intricate, but very
  -- similar to checking a function, followed by checking a call to
  -- it.  The overall procedure is as follows:
  --
  -- (1) All empty dimensions in the merge pattern are instantiated
  -- with nonrigid size variables.  All explicitly specified
  -- dimensions are preserved.
  --
  -- (2) The body of the loop is type-checked.  The result type is
  -- combined with the merge pattern type to determine which sizes are
  -- variant, and these are turned into size parameters for the merge
  -- pattern.
  --
  -- (3) We now conceptually have a function parameter type and
  -- return type.  We check that it can be called with the body type
  -- as argument.
  --
  -- (4) Similarly to (3), we check that the "function" can be
  -- called with the initial merge values as argument.  The result
  -- of this is the type of the loop as a whole.

  (merge_t, new_dims_map) <-
    -- dim handling (1)
    allDimsFreshInType (mkUsage loc "loop parameter type inference") Nonrigid "loop_d"
      =<< expTypeFully mergeexp'
  let new_dims_to_initial_dim = M.toList new_dims_map
      new_dims = map fst new_dims_to_initial_dim

  -- dim handling (2)
  let checkLoopReturnSize mergepat' loopbody' = do
        loopbody_t <- expTypeFully loopbody'
        mergepat_t <- normTypeFully (patternType mergepat')

        let ok_names = known_before <> S.fromList new_dims
        checkForImpossible (locOf mergepat) ok_names mergepat_t

        pat_t <- someDimsFreshInType loc "loop" new_dims mergepat_t

        -- We are ignoring the dimensions here, because any mismatches
        -- should be turned into fresh size variables.
        onFailure (CheckingLoopBody (toStruct pat_t) (toStruct loopbody_t)) $
          unify
            (mkUsage loopbody "matching loop body to loop pattern")
            (toStruct pat_t)
            (toStruct loopbody_t)

        -- Figure out which of the 'new_dims' dimensions are variant.
        -- This works because we know that each dimension from
        -- new_dims in the pattern is unique and distinct.
        areSameSize <- getAreSame
        let onDims _ x y
              | x == y = pure x
            onDims _ e d = do
              forM_ (fvVars $ freeInExp e) $ \v -> do
                case L.find (areSameSize v . fst) new_dims_to_initial_dim of
                  Just (_, e') ->
                    if e' == d
                      then modify $ first $ M.insert v $ ExpSubst e'
                      else
                        unless (v `S.member` known_before) $
                          modify (second (v :))
                  _ ->
                    pure ()
              pure e
        loopbody_t' <- normTypeFully loopbody_t
        merge_t' <- normTypeFully merge_t

        let (init_substs, sparams) =
              execState (matchDims onDims merge_t' loopbody_t') mempty

        -- Make sure that any of new_dims that are invariant will be
        -- replaced with the invariant size in the loop body.  Failure
        -- to do this can cause type annotations to still refer to
        -- new_dims.
        let dimToInit (v, ExpSubst e) =
              constrain v $ Size (Just e) (mkUsage loc "size of loop parameter")
            dimToInit _ =
              pure ()
        mapM_ dimToInit $ M.toList init_substs

        mergepat'' <- applySubst (`M.lookup` init_substs) <$> updateTypes mergepat'

        -- Eliminate those new_dims that turned into sparams so it won't
        -- look like we have ambiguous sizes lying around.
        modifyConstraints $ M.filterWithKey $ \k _ -> k `notElem` sparams

        -- dim handling (3)
        --
        -- The only trick here is that we have to turn any instances
        -- of loop parameters in the type of loopbody' rigid,
        -- because we are no longer in a position to change them,
        -- really.
        wellTypedLoopArg BodyResult sparams mergepat'' loopbody'

        pure (nubOrd sparams, mergepat'')

  (sparams, mergepat', form', loopbody') <-
    case form of
      For i uboundexp -> do
        uboundexp' <-
          require "being the bound in a 'for' loop" anySignedType
            =<< checkExp uboundexp
        bound_t <- expTypeFully uboundexp'
        bindingIdent i bound_t $ \i' ->
          bindingPat [] mergepat merge_t $ \mergepat' -> incLevel $ do
            loopbody' <- checkExp loopbody
            (sparams, mergepat'') <- checkLoopReturnSize mergepat' loopbody'
            pure
              ( sparams,
                mergepat'',
                For i' uboundexp',
                loopbody'
              )
      ForIn xpat e -> do
        (arr_t, _) <- newArrayType (mkUsage' (srclocOf e)) "e" 1
        e' <- unifies "being iterated in a 'for-in' loop" arr_t =<< checkExp e
        t <- expTypeFully e'
        case t of
          _
            | Just t' <- peelArray 1 t ->
                bindingPat [] xpat t' $ \xpat' ->
                  bindingPat [] mergepat merge_t $ \mergepat' -> incLevel $ do
                    loopbody' <- checkExp loopbody
                    (sparams, mergepat'') <- checkLoopReturnSize mergepat' loopbody'
                    pure
                      ( sparams,
                        mergepat'',
                        ForIn (fmap toStruct xpat') e',
                        loopbody'
                      )
            | otherwise ->
                typeError (srclocOf e) mempty $
                  "Iteratee of a for-in loop must be an array, but expression has type"
                    <+> pretty t
      While cond ->
        bindingPat [] mergepat merge_t $ \mergepat' ->
          incLevel $ do
            cond' <-
              checkExp cond
                >>= unifies "being the condition of a 'while' loop" (Scalar $ Prim Bool)
            loopbody' <- checkExp loopbody
            (sparams, mergepat'') <- checkLoopReturnSize mergepat' loopbody'
            pure
              ( sparams,
                mergepat'',
                While cond',
                loopbody'
              )

  -- dim handling (4)
  wellTypedLoopArg Initial sparams mergepat' mergeexp'

  (loopt, retext) <-
    freshDimsInType
      (mkUsage loc "inference of loop result type")
      (Rigid RigidLoop)
      "loop"
      sparams
      (patternType mergepat')
  pure
    ( (sparams, mergepat', mergeexp', form', loopbody'),
      AppRes (toStruct loopt) retext
    )
