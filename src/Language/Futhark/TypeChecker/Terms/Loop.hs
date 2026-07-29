-- | Size inference of @loop@. This is complicated, so the implementation is
-- separate from the main type checker.
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

-- | How the loop body produces the size at a given loop parameter position,
-- relative to the fresh variable standing for that position's size. See Note
-- [Loop size inference].
data DimClass
  = -- | A genuinely new size: this position is variant.
    Fresh
  | -- | The body produces this position's initial size (the size the fresh
    -- variable replaced). Either it produces that size directly (@Nothing@), or
    -- it copies the /current/ size of another parameter @u@ that started at the
    -- same initial size (@Just u@). In the latter case the two sizes coincide
    -- only while @u@ is unchanged, so this position is invariant exactly when
    -- @u@ is.
    Reproduces (Maybe VName)

data ArgSource = Initial | BodyResult

wellTypedLoopArg :: ArgSource -> [VName] -> Pat ParamType -> Exp -> TermTypeM ()
wellTypedLoopArg src sparams pat arg = do
  (loop_t, _) <-
    freshDimsInType (mkUsage arg desc) Nonrigid "loop" sparams $
      toStruct (patternType pat)
  arg_t <- toStruct <$> expTypeFully arg
  onFailure (checking loop_t arg_t) $
    unify (mkUsage arg desc) loop_t arg_t
  where
    (checking, desc) =
      case src of
        Initial -> (CheckingLoopInitial, "matching initial loop values to pattern")
        BodyResult -> (CheckingLoopBody, "matching loop body to pattern")

-- | An un-checked loop.
type UncheckedLoop =
  (Pat ParamType, LoopInitBase Info VName, LoopFormBase Info VName, Exp)

-- | A loop that has been type-checked.
type CheckedLoop =
  ([VName], Pat ParamType, LoopInitBase Info VName, LoopFormBase Info VName, Exp)

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

-- Check if any sparams have reduced levels, which indicates they escaped the
-- loop scope and got constrained by something at a lower (outer) level. This
-- means they cannot actually be variant across the loop.
checkForEscaped :: SrcLoc -> M.Map VName Level -> [VName] -> TermTypeM ()
checkForEscaped loc initial_levels sparams = do
  cs <- getConstraints
  let checkSparamLevel sparam = do
        let current_level = fst <$> M.lookup sparam cs
            original_level = M.lookup sparam initial_levels
        case (original_level, current_level) of
          (Just orig, Just curr)
            | curr < orig ->
                Just
                  . typeError loc mempty
                  . withIndexLink "loop-variant-escape"
                  $ "Loop-variant size"
                    <+> "has escaped into type that occurs outside of loop."
                    </> "This is likely because the loop body uses a variable whose type"
                    </> "depends on the loop parameter."
          _ -> Nothing
  case mapMaybe checkSparamLevel sparams of
    problem : _ -> problem
    [] -> pure ()

-- | Type-check a @loop@ expression, passing in a function for
-- type-checking subexpressions.  See Note [Loop size inference].
checkLoop ::
  (Exp -> TermTypeM Exp) ->
  UncheckedLoop ->
  SrcLoc ->
  TermTypeM (CheckedLoop, AppRes)
checkLoop checkExp (looppat, loopinit, form, loopbody) loc = do
  loopinit' <- checkExp $ loopInitExp loopinit
  known_before <- M.keysSet <$> getConstraints

  (loop_t, new_dims_map) <-
    -- dim handling (1)
    allDimsFreshInType
      (mkUsage loc "loop parameter type inference")
      Nonrigid
      "loop_d"
      =<< expTypeFully loopinit'
  let new_dims_to_initial_dim = M.toList new_dims_map
      new_dims = map fst new_dims_to_initial_dim

  -- Save the initial levels of new_dims for later checking
  initial_levels <- do
    cs <- getConstraints
    pure $ M.fromList [(v, lvl) | v <- new_dims, Just (lvl, _) <- [M.lookup v cs]]

  -- dim handling (2)
  let checkLoopReturnSize looppat' loopbody' = do
        loopbody_t <- expTypeFully loopbody'
        looppat_t <- normTypeFully (patternType looppat')

        let ok_names = known_before <> S.fromList new_dims
        checkForImpossible (locOf looppat) ok_names looppat_t

        pat_t <- someDimsFreshInType loc "loop" new_dims looppat_t

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
        let initialOf v = snd <$> L.find (areSameSize v . fst) new_dims_to_initial_dim
            sameSize (Var x _ _) (Var y _ _) = areSameSize (qualLeaf x) (qualLeaf y)
            sameSize x y = x == y
            -- If the body-produced size 'd' is another loop parameter that
            -- shares the initial size 'e'', return that parameter.
            sharesInitial e' (Var d _ _)
              | Just d_init <- initialOf (qualLeaf d),
                sameSize d_init e' =
                  Just $ qualLeaf d
            sharesInitial _ _ = Nothing
            -- Classify each new_dim 'v' (with initial size 'e'') by how the
            -- loop body produces the corresponding size 'd'.
            onDims _ x y
              | x == y = pure x
            onDims _ e d = do
              forM_ (fvVars $ freeInExp e) $ \v ->
                case initialOf v of
                  Just e'
                    | sameSize e' d ->
                        modify $ M.insert v $ Reproduces Nothing
                    | v `elem` new_dims,
                      Just u <- sharesInitial e' d ->
                        modify $ M.insert v $ Reproduces (Just u)
                    | not $ v `S.member` known_before ->
                        modify $ M.insert v Fresh
                    | otherwise ->
                        pure ()
                  Nothing ->
                    pure ()
              pure e
        loopbody_t' <- normTypeFully loopbody_t
        loop_t' <- normTypeFully loop_t

        let classified =
              execState (matchDims onDims loop_t' loopbody_t') mempty
            -- The variant sizes are the least set containing every 'Fresh'
            -- position and closed under the copies-from dependency.  See Note
            -- [Loop size inference].
            seeds = S.fromList [v | (v, Fresh) <- M.toList classified]
            grow vs =
              vs
                <> S.fromList
                  [ v
                  | (v, Reproduces (Just u)) <- M.toList classified,
                    u `S.member` vs
                  ]
            fixVariant vs =
              let vs' = grow vs
               in if vs' == vs then vs else fixVariant vs'
            variant = fixVariant seeds
            -- The invariant positions are resolved back to the initial size
            -- they stand for.
            init_substs =
              M.fromList $ do
                (v, Reproduces _) <- M.toList classified
                guard $ v `S.notMember` variant
                Just e' <- [initialOf v]
                pure (v, ExpSubst e')
            sparams = S.toList variant

        checkForEscaped loc initial_levels sparams

        -- Make sure that any of new_dims that are invariant will be
        -- replaced with the invariant size in the loop body.  Failure
        -- to do this can cause type annotations to still refer to
        -- new_dims.
        let dimToInit (v, ExpSubst e) =
              constrain v $ Size (Just e) (mkUsage loc "size of loop parameter")
            dimToInit _ =
              pure ()
        mapM_ dimToInit $ M.toList init_substs

        looppat'' <- applySubst (`M.lookup` init_substs) <$> updateTypes looppat'

        -- Eliminate those new_dims that turned into sparams so it won't
        -- look like we have ambiguous sizes lying around.
        modifyConstraints $ M.filterWithKey $ \k _ -> k `notElem` sparams

        -- dim handling (3)
        --
        -- The only trick here is that we have to turn any instances
        -- of loop parameters in the type of loopbody' rigid,
        -- because we are no longer in a position to change them,
        -- really.
        wellTypedLoopArg BodyResult sparams looppat'' loopbody'

        pure (nubOrd sparams, looppat'')

  (sparams, looppat', form', loopbody') <-
    case form of
      For i uboundexp -> do
        uboundexp' <- checkExp uboundexp
        it <- expType uboundexp'
        let i' = i {identType = Info it}
        bindingIdent i' $
          -- Note: bindingParam, not bindingPat, because we must
          -- preserve the Diet of the pattern as determined by the
          -- unsized type checker (the loop parameter may be
          -- consumable).
          bindingParam looppat loop_t $ \looppat' -> incLevel $ do
            loopbody' <- checkExp loopbody
            (sparams, looppat'') <- checkLoopReturnSize looppat' loopbody'
            pure
              ( sparams,
                looppat'',
                For i' uboundexp',
                loopbody'
              )
      ForIn xpat e -> do
        e' <- checkExp e
        t <- expTypeFully e'
        case t of
          _
            | Just t' <- peelArray 1 t ->
                bindingPat [] xpat t' $ \xpat' ->
                  bindingParam looppat loop_t $ \looppat' -> incLevel $ do
                    loopbody' <- checkExp loopbody
                    (sparams, looppat'') <- checkLoopReturnSize looppat' loopbody'
                    pure
                      ( sparams,
                        looppat'',
                        ForIn (fmap toStruct xpat') e',
                        loopbody'
                      )
            | otherwise ->
                typeError (srclocOf e) mempty $
                  "Iteratee of a for-in loop must be an array, but expression has type"
                    <+> pretty t
      While cond ->
        bindingParam looppat loop_t $ \looppat' ->
          incLevel $ do
            cond' <-
              checkExp cond
                >>= unifies "being the condition of a 'while' loop" (Scalar $ Prim Bool)
            loopbody' <- checkExp loopbody
            (sparams, looppat'') <- checkLoopReturnSize looppat' loopbody'
            pure
              ( sparams,
                looppat'',
                While cond',
                loopbody'
              )

  -- dim handling (4)
  wellTypedLoopArg Initial sparams looppat' loopinit'

  (loopt, retext) <-
    freshDimsInType
      (mkUsage loc "inference of loop result type")
      (Rigid RigidLoop)
      "loop"
      sparams
      (patternType looppat')
  pure
    ( (sparams, looppat', LoopInitExplicit loopinit', form', loopbody'),
      AppRes (toStruct loopt) retext
    )

-- Note [Loop size inference]
--
-- A loop
--
--   loop p = e_init (while/for ...) do e_body
--
-- behaves like defining a function @f p = e_body@ and then calling it
-- repeatedly as @f (f ... (f e_init))@. Size-checking mirrors that: for each
-- dimension of the loop parameter @p@ we must infer whether it stays fixed
-- across iterations (*invariant*, so it can name a size that is meaningful
-- outside the loop) or may change (*variant*, so it must be existentially
-- quantified in the loop's result type). This is done in four steps, tagged
-- "dim handling (1)".."(4)" in 'checkLoop':
--
-- (1) Instantiate every size in the type of @e_init@ with a fresh nonrigid
--     variable (via 'allDimsFreshInType'), giving @loop_t@ and a map from each
--     fresh variable to the initial size it replaced. These fresh variables
--     ('new_dims') are the sizes whose variance we must determine. Distinct
--     occurrences get distinct variables even when they replace the same
--     initial size, so that e.g. two parameters that both start out @[n]@ can
--     still evolve independently.
--
-- (2) Check @e_body@ with @p@ bound at type @loop_t@, then compare the body's
--     result type against the parameter type dimension-by-dimension (with
--     'matchDims' and 'onDims'). For a new_dim @v@ that replaced initial size
--     @e'@, look at the size @d@ the body produced in that same position and
--     classify @v@ ('DimClass'):
--
--       * If @d@ is @e'@ itself, then @v@ is invariant (@Reproduces Nothing@).
--
--       * If @d@ is another new_dim @u@ that shares @v@'s initial size, then
--         @v@ is invariant iff @u@ is (@Reproduces (Just u)@). This is the
--         swap/rotation case: parameters exchanging equally-sized arrays stay
--         fixed, but if an array one of them receives has been resized, they
--         become variant.
--
--       * If @d@ is a genuinely new size, then @v@ is variant (@Fresh@).
--
--     Variance can be mutual (in @loop (a,b) = ... in (b,a)@ each of @a@,@b@
--     copies the other's size), so we take the variant set to be the least set
--     that contains every @Fresh@ position and is closed under the copies-from
--     dependency. Parameters that only copy from one another, with no @Fresh@
--     seed feeding the cycle, are therefore never made variant -- a plain swap
--     of two @[n]@ arrays keeps its precise size, while resizing one of them
--     makes both existential. The variant new_dims become the loop's size
--     parameters ('sparams'); the invariant ones are substituted back to the
--     initial size they stand for.
--
-- (3) We now conceptually have a function parameter and return type.  Check,
--     as if calling that function, that the parameter type admits the body
--     result.
--
-- (4) Likewise check that it admits the initial values; this yields the loop's
--     overall (rigid) result type.
