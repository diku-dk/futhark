{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Type inference of @loop@.  This is complicated because of the
-- uniqueness and size inference, so the implementation is separate
-- from the main type checker.
module Language.Futhark.TypeChecker.Terms.DoLoop
  ( UncheckedLoop,
    CheckedLoop,
    checkDoLoop,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Bitraversable
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Futhark.Util (nubOrd)
import Futhark.Util.Pretty hiding (bool, group, space)
import Language.Futhark
import Language.Futhark.TypeChecker.Monad hiding (BoundV)
import Language.Futhark.TypeChecker.Terms.Monad hiding (consumed)
import Language.Futhark.TypeChecker.Terms.Pat
import Language.Futhark.TypeChecker.Types
import Language.Futhark.TypeChecker.Unify
import Prelude hiding (mod)

-- | Replace specified sizes with distinct fresh size variables.
someDimsFreshInType ::
  SrcLoc ->
  Rigidity ->
  Name ->
  S.Set VName ->
  TypeBase (DimDecl VName) als ->
  TermTypeM (TypeBase (DimDecl VName) als)
someDimsFreshInType loc r desc sizes = bitraverse onDim pure
  where
    onDim (NamedDim d)
      | qualLeaf d `S.member` sizes = do
          v <- newDimVar loc r desc
          pure $ NamedDim $ qualName v
    onDim d = pure d

-- | Replace the specified sizes with fresh size variables of the
-- specified ridigity.  Returns the new fresh size variables.
freshDimsInType ::
  SrcLoc ->
  Rigidity ->
  Name ->
  S.Set VName ->
  TypeBase (DimDecl VName) als ->
  TermTypeM (TypeBase (DimDecl VName) als, [VName])
freshDimsInType loc r desc sizes t =
  second M.elems <$> runStateT (bitraverse onDim pure t) mempty
  where
    onDim (NamedDim d)
      | qualLeaf d `S.member` sizes = do
          prev_subst <- gets $ M.lookup $ qualLeaf d
          case prev_subst of
            Just d' -> pure $ NamedDim $ qualName d'
            Nothing -> do
              v <- lift $ newDimVar loc r desc
              modify $ M.insert (qualLeaf d) v
              pure $ NamedDim $ qualName v
    onDim d = pure d

-- | Mark bindings of names in "consumed" as Unique.
uniquePat :: Names -> Pat -> Pat
uniquePat consumed = recurse
  where
    recurse (Wildcard (Info t) wloc) =
      Wildcard (Info $ t `setUniqueness` Nonunique) wloc
    recurse (PatParens p ploc) =
      PatParens (recurse p) ploc
    recurse (PatAttr attr p ploc) =
      PatAttr attr (recurse p) ploc
    recurse (Id name (Info t) iloc)
      | name `S.member` consumed =
          let t' = t `setUniqueness` Unique `setAliases` mempty
           in Id name (Info t') iloc
      | otherwise =
          let t' = t `setUniqueness` Nonunique
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

convergePat :: SrcLoc -> Pat -> Names -> PatType -> Usage -> TermTypeM Pat
convergePat loop_loc pat body_cons body_t body_loc = do
  let -- Make the pattern unique where needed.
      pat' = uniquePat (patNames pat `S.intersection` body_cons) pat

  -- Check that the new values of consumed merge parameters do not
  -- alias something bound outside the loop, AND that anything
  -- returned for a unique merge parameter does not alias anything
  -- else returned.  We also update the aliases for the pattern.
  bound_outside <- asks $ S.fromList . M.keys . scopeVtable . termScope
  let combAliases t1 t2 =
        case t1 of
          Scalar Record {} -> t1
          _ -> t1 `addAliases` (<> aliases t2)

      checkMergeReturn (Id pat_v (Info pat_v_t) patloc) t
        | unique pat_v_t,
          v : _ <-
            S.toList $
              S.map aliasVar (aliases t) `S.intersection` bound_outside =
            lift . typeError loop_loc mempty $
              "Return value for loop parameter"
                <+> pquote (pprName pat_v)
                <+> "aliases"
                <+> pquote (pprName v) <> "."
        | otherwise = do
            (cons, obs) <- get
            unless (S.null $ aliases t `S.intersection` cons) $
              lift . typeError loop_loc mempty $
                "Return value for loop parameter"
                  <+> pquote (pprName pat_v)
                  <+> "aliases other consumed loop parameter."
            when
              ( unique pat_v_t
                  && not (S.null (aliases t `S.intersection` (cons <> obs)))
              )
              $ lift . typeError loop_loc mempty $
                "Return value for consuming loop parameter"
                  <+> pquote (pprName pat_v)
                  <+> "aliases previously returned value."
            if unique pat_v_t
              then put (cons <> aliases t, obs)
              else put (cons, obs <> aliases t)

            pure $ Id pat_v (Info (combAliases pat_v_t t)) patloc
      checkMergeReturn (Wildcard (Info pat_v_t) patloc) t =
        pure $ Wildcard (Info (combAliases pat_v_t t)) patloc
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

  (pat'', (pat_cons, _)) <-
    runStateT (checkMergeReturn pat' body_t) (mempty, mempty)

  let body_cons' = body_cons <> S.map aliasVar pat_cons
  if body_cons' == body_cons && patternType pat'' == patternType pat
    then pure pat'
    else convergePat loop_loc pat'' body_cons' body_t body_loc

data ArgSource = Initial | BodyResult

wellTypedLoopArg :: ArgSource -> [VName] -> Pat -> Exp -> TermTypeM ()
wellTypedLoopArg src sparams pat arg = do
  (merge_t, _) <-
    freshDimsInType (srclocOf arg) Nonrigid "loop" (S.fromList sparams) $
      toStruct $ patternType pat
  arg_t <- toStruct <$> expTypeFully arg
  onFailure (checking merge_t arg_t) $
    unify
      (mkUsage (srclocOf arg) desc)
      merge_t
      arg_t
  where
    (checking, desc) =
      case src of
        Initial -> (CheckingLoopInitial, "matching initial loop values to pattern")
        BodyResult -> (CheckingLoopBody, "matching loop body to pattern")

-- | An un-checked loop.
type UncheckedLoop =
  (UncheckedPat, UncheckedExp, LoopFormBase NoInfo Name, UncheckedExp)

-- | A loop that has been type-checked.
type CheckedLoop =
  ([VName], Pat, Exp, LoopFormBase Info VName, Exp)

-- | Type-check a @loop@ expression, passing in a function for
-- type-checking subexpressions.
checkDoLoop ::
  (UncheckedExp -> TermTypeM Exp) ->
  UncheckedLoop ->
  SrcLoc ->
  TermTypeM (CheckedLoop, AppRes)
checkDoLoop checkExp (mergepat, mergeexp, form, loopbody) loc =
  sequentially (checkExp mergeexp) $ \mergeexp' _ -> do
    zeroOrderType
      (mkUsage (srclocOf mergeexp) "use as loop variable")
      "type used as loop variable"
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
    --
    -- (There is also a convergence loop for inferring uniqueness, but
    -- that's orthogonal to the size handling.)

    (merge_t, new_dims_to_initial_dim) <-
      -- dim handling (1)
      allDimsFreshInType loc Nonrigid "loop" =<< expTypeFully mergeexp'
    let new_dims = M.keys new_dims_to_initial_dim

    -- dim handling (2)
    let checkLoopReturnSize mergepat' loopbody' = do
          loopbody_t <- expTypeFully loopbody'
          pat_t <-
            someDimsFreshInType loc Nonrigid "loop" (S.fromList new_dims)
              =<< normTypeFully (patternType mergepat')

          -- We are ignoring the dimensions here, because any mismatches
          -- should be turned into fresh size variables.
          onFailure (CheckingLoopBody (toStruct pat_t) (toStruct loopbody_t)) $
            unify
              (mkUsage (srclocOf loopbody) "matching loop body to loop pattern")
              (toStruct pat_t)
              (toStruct loopbody_t)

          -- Figure out which of the 'new_dims' dimensions are variant.
          -- This works because we know that each dimension from
          -- new_dims in the pattern is unique and distinct.
          let onDims _ x y
                | x == y = pure x
              onDims _ (NamedDim v) d
                | qualLeaf v `elem` new_dims = do
                    case M.lookup (qualLeaf v) new_dims_to_initial_dim of
                      Just d'
                        | d' == d ->
                            modify $ first $ M.insert (qualLeaf v) (SizeSubst d)
                      _ ->
                        modify $ second (qualLeaf v :)
                    pure $ NamedDim v
              onDims _ x _ = pure x
          loopbody_t' <- normTypeFully loopbody_t
          merge_t' <- normTypeFully merge_t
          let (init_substs, sparams) =
                execState (matchDims onDims merge_t' loopbody_t') mempty

          -- Make sure that any of new_dims that are invariant will be
          -- replaced with the invariant size in the loop body.  Failure
          -- to do this can cause type annotations to still refer to
          -- new_dims.
          let dimToInit (v, SizeSubst d) =
                constrain v $ Size (Just d) (mkUsage loc "size of loop parameter")
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

    -- First we do a basic check of the loop body to figure out which of
    -- the merge parameters are being consumed.  For this, we first need
    -- to check the merge pattern, which requires the (initial) merge
    -- expression.
    --
    -- Play a little with occurences to ensure it does not look like
    -- none of the merge variables are being used.
    ((sparams, mergepat', form', loopbody'), bodyflow) <-
      case form of
        For i uboundexp -> do
          uboundexp' <-
            require "being the bound in a 'for' loop" anySignedType
              =<< checkExp uboundexp
          bound_t <- expTypeFully uboundexp'
          bindingIdent i bound_t $ \i' ->
            noUnique . bindingPat [] mergepat (Ascribed merge_t) $
              \mergepat' -> onlySelfAliasing . tapOccurrences $ do
                loopbody' <- noSizeEscape $ checkExp loopbody
                (sparams, mergepat'') <- checkLoopReturnSize mergepat' loopbody'
                pure
                  ( sparams,
                    mergepat'',
                    For i' uboundexp',
                    loopbody'
                  )
        ForIn xpat e -> do
          (arr_t, _) <- newArrayType (srclocOf e) "e" 1
          e' <- unifies "being iterated in a 'for-in' loop" arr_t =<< checkExp e
          t <- expTypeFully e'
          case t of
            _
              | Just t' <- peelArray 1 t ->
                  bindingPat [] xpat (Ascribed t') $ \xpat' ->
                    noUnique . bindingPat [] mergepat (Ascribed merge_t) $
                      \mergepat' -> onlySelfAliasing . tapOccurrences $ do
                        loopbody' <- noSizeEscape $ checkExp loopbody
                        (sparams, mergepat'') <- checkLoopReturnSize mergepat' loopbody'
                        pure
                          ( sparams,
                            mergepat'',
                            ForIn xpat' e',
                            loopbody'
                          )
              | otherwise ->
                  typeError (srclocOf e) mempty $
                    "Iteratee of a for-in loop must be an array, but expression has type"
                      <+> ppr t
        While cond ->
          noUnique . bindingPat [] mergepat (Ascribed merge_t) $ \mergepat' ->
            onlySelfAliasing . tapOccurrences
              . sequentially
                ( checkExp cond
                    >>= unifies "being the condition of a 'while' loop" (Scalar $ Prim Bool)
                )
              $ \cond' _ -> do
                loopbody' <- noSizeEscape $ checkExp loopbody
                (sparams, mergepat'') <- checkLoopReturnSize mergepat' loopbody'
                pure
                  ( sparams,
                    mergepat'',
                    While cond',
                    loopbody'
                  )

    mergepat'' <- do
      loopbody_t <- expTypeFully loopbody'
      convergePat loc mergepat' (allConsumed bodyflow) loopbody_t $
        mkUsage (srclocOf loopbody') "being (part of) the result of the loop body"

    let consumeMerge (Id _ (Info pt) ploc) mt
          | unique pt = consume ploc $ aliases mt
        consumeMerge (TuplePat pats _) t
          | Just ts <- isTupleRecord t =
              zipWithM_ consumeMerge pats ts
        consumeMerge (PatParens pat _) t =
          consumeMerge pat t
        consumeMerge (PatAscription pat _ _) t =
          consumeMerge pat t
        consumeMerge _ _ =
          pure ()
    consumeMerge mergepat'' =<< expTypeFully mergeexp'

    -- dim handling (4)
    wellTypedLoopArg Initial sparams mergepat'' mergeexp'

    (loopt, retext) <-
      freshDimsInType loc (Rigid RigidLoop) "loop" (S.fromList sparams) $
        patternType mergepat''
    -- We set all of the uniqueness to be unique.  This is intentional,
    -- and matches what happens for function calls.  Those arrays that
    -- really *cannot* be consumed will alias something unconsumable,
    -- and will be caught that way.
    let bound_here = patNames mergepat'' <> S.fromList sparams <> form_bound
        form_bound =
          case form' of
            For v _ -> S.singleton $ identName v
            ForIn forpat _ -> patNames forpat
            While {} -> mempty
        loopt' =
          second (`S.difference` S.map AliasBound bound_here) $
            loopt `setUniqueness` Unique

    pure ((sparams, mergepat'', mergeexp', form', loopbody'), AppRes loopt' retext)
