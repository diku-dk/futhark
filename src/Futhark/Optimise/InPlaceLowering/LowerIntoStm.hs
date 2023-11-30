{-# LANGUAGE TypeFamilies #-}

module Futhark.Optimise.InPlaceLowering.LowerIntoStm
  ( lowerUpdateGPU,
    lowerUpdate,
    LowerUpdate,
    DesiredUpdate (..),
  )
where

import Control.Monad
import Control.Monad.Writer
import Data.Either
import Data.List (find, unzip5)
import Data.Maybe (isNothing, mapMaybe)
import Futhark.Analysis.PrimExp.Convert
import Futhark.Construct
import Futhark.IR.Aliases
import Futhark.IR.GPU
import Futhark.Optimise.InPlaceLowering.SubstituteIndices

data DesiredUpdate dec = DesiredUpdate
  { -- | Name of result.
    updateName :: VName,
    -- | Type of result.
    updateType :: dec,
    updateCerts :: Certs,
    updateSource :: VName,
    updateIndices :: Slice SubExp,
    updateValue :: VName
  }
  deriving (Show)

instance Functor DesiredUpdate where
  f `fmap` u = u {updateType = f $ updateType u}

updateHasValue :: VName -> DesiredUpdate dec -> Bool
updateHasValue name = (name ==) . updateValue

type LowerUpdate rep m =
  Scope (Aliases rep) ->
  Stm (Aliases rep) ->
  [DesiredUpdate (LetDec (Aliases rep))] ->
  Maybe (m [Stm (Aliases rep)])

lowerUpdate ::
  ( MonadFreshNames m,
    Buildable rep,
    LetDec rep ~ Type,
    AliasableRep rep
  ) =>
  LowerUpdate rep m
lowerUpdate scope (Let pat aux (Loop merge form body)) updates = do
  canDo <- lowerUpdateIntoLoop scope updates pat merge form body
  Just $ do
    (prestms, poststms, pat', merge', body') <- canDo
    pure $
      prestms
        ++ [ certify (stmAuxCerts aux) $
               mkLet pat' $
                 Loop merge' form body'
           ]
        ++ poststms
lowerUpdate
  _
  (Let pat aux (BasicOp (SubExp (Var v))))
  [DesiredUpdate bindee_nm bindee_dec cs src (Slice is) val]
    | patNames pat == [src] =
        let is' = fullSlice (typeOf bindee_dec) is
         in Just . pure $
              [ certify (stmAuxCerts aux <> cs) $
                  mkLet [Ident bindee_nm $ typeOf bindee_dec] $
                    BasicOp $
                      Update Unsafe v is' $
                        Var val
              ]
lowerUpdate _ _ _ =
  Nothing

lowerUpdateGPU :: (MonadFreshNames m) => LowerUpdate GPU m
lowerUpdateGPU
  scope
  (Let pat aux (Op (SegOp (SegMap lvl space ts kbody))))
  updates
    | all ((`elem` patNames pat) . updateValue) updates,
      not source_used_in_kbody = do
        mk <- lowerUpdatesIntoSegMap scope pat ts updates space kbody
        Just $ do
          (pat', ts', kbody', poststms) <- mk
          let cs = stmAuxCerts aux <> foldMap updateCerts updates
          pure $
            certify cs (Let pat' aux $ Op $ SegOp $ SegMap lvl space ts' kbody')
              : stmsToList poststms
    where
      -- This check is a bit more conservative than ideal.  In a perfect
      -- world, we would allow indexing a[i,j] if the update is also
      -- to exactly a[i,j], as that will not create cross-iteration
      -- dependencies.  (Although the type checker wouldn't be able to
      -- permit this anyway.)
      source_used_in_kbody =
        mconcat (map (`lookupAliases` scope) (namesToList (freeIn kbody)))
          `namesIntersect` mconcat (map ((`lookupAliases` scope) . updateSource) updates)
lowerUpdateGPU scope stm updates = lowerUpdate scope stm updates

lowerUpdatesIntoSegMap ::
  (MonadFreshNames m) =>
  Scope (Aliases GPU) ->
  Pat (LetDec (Aliases GPU)) ->
  [Type] ->
  [DesiredUpdate (LetDec (Aliases GPU))] ->
  SegSpace ->
  KernelBody (Aliases GPU) ->
  Maybe
    ( m
        ( Pat (LetDec (Aliases GPU)),
          [Type],
          KernelBody (Aliases GPU),
          Stms (Aliases GPU)
        )
    )
lowerUpdatesIntoSegMap scope pat ret_ts updates kspace kbody = do
  -- The updates are all-or-nothing.  Being more liberal would require
  -- changes to the in-place-lowering pass itself.
  mk <- mapM onRet (zip3 (patElems pat) ret_ts (kernelBodyResult kbody))
  pure $ do
    (pes, ret_ts', bodystms, krets, poststms) <- unzip5 <$> sequence mk
    pure
      ( Pat pes,
        ret_ts',
        kbody
          { kernelBodyStms = kernelBodyStms kbody <> mconcat bodystms,
            kernelBodyResult = krets
          },
        mconcat poststms
      )
  where
    (gtids, _dims) = unzip $ unSegSpace kspace

    onRet (PatElem v v_dec, _, ret)
      | Just (DesiredUpdate bindee_nm bindee_dec _cs src slice _val) <-
          find ((== v) . updateValue) updates = do
          Returns _ cs se <- Just ret

          -- The slice we're writing per thread must fully cover the
          -- underlying dimensions.
          guard $
            let (dims', slice') =
                  unzip . drop (length gtids) . filter (isNothing . dimFix . snd) $
                    zip (arrayDims (typeOf bindee_dec)) (unSlice slice)
             in isFullSlice (Shape dims') (Slice slice')

          Just $ do
            (slice', bodystms) <-
              flip runBuilderT scope $
                traverse (toSubExp "index") $
                  fixSlice (fmap pe64 slice) $
                    map (pe64 . Var) gtids

            let ret' = WriteReturns cs src [(fullSlice (typeOf bindee_dec) (map DimFix slice'), se)]

            v_aliased <- newName v

            pure
              ( PatElem bindee_nm bindee_dec,
                typeOf bindee_dec,
                bodystms,
                ret',
                stmsFromList
                  [ mkLet [Ident v_aliased $ typeOf v_dec] $ BasicOp $ Index bindee_nm slice,
                    mkLet [Ident v $ typeOf v_dec] $ BasicOp $ Replicate mempty $ Var v_aliased
                  ]
              )
    onRet (pe, ret_t, ret) =
      Just $ pure (pe, ret_t, mempty, ret, mempty)

lowerUpdateIntoLoop ::
  ( Buildable rep,
    BuilderOps rep,
    Aliased rep,
    LetDec rep ~ (als, Type),
    MonadFreshNames m
  ) =>
  Scope rep ->
  [DesiredUpdate (LetDec rep)] ->
  Pat (LetDec rep) ->
  [(FParam rep, SubExp)] ->
  LoopForm ->
  Body rep ->
  Maybe
    ( m
        ( [Stm rep],
          [Stm rep],
          [Ident],
          [(FParam rep, SubExp)],
          Body rep
        )
    )
lowerUpdateIntoLoop scope updates pat val form body = do
  -- Algorithm:
  --
  --   0) Map each result of the loop body to a corresponding in-place
  --      update, if one exists.
  --
  --   1) Create new merge variables corresponding to the arrays being
  --      updated; extend the pattern and the @res@ list with these,
  --      and remove the parts of the result list that have a
  --      corresponding in-place update.
  --
  --      (The creation of the new merge variable identifiers is
  --      actually done at the same time as step (0)).
  --
  --   2) Create in-place updates at the end of the loop body.
  --
  --   3) Create index expressions that read back the values written
  --      in (2).  If the merge parameter corresponding to this value
  --      is unique, also @copy@ this value.
  --
  --   4) Update the result of the loop body to properly pass the new
  --      arrays and indexed elements to the next iteration of the
  --      loop.
  --
  -- We also check that the merge parameters we work with have
  -- loop-invariant shapes.

  -- Safety condition (8).
  forM_ (zip val $ bodyAliases body) $ \((p, _), als) ->
    guard $ paramName p `notNameIn` als

  mk_in_place_map <- summariseLoop scope updates usedInBody resmap val

  Just $ do
    in_place_map <- mk_in_place_map
    (val', prestms, poststms) <- mkMerges in_place_map
    let valpat = mkResAndPat in_place_map
        idxsubsts = indexSubstitutions in_place_map
    (idxsubsts', newstms) <- substituteIndices idxsubsts $ bodyStms body
    (body_res, res_stms) <- manipulateResult in_place_map idxsubsts'
    let body' = mkBody (newstms <> res_stms) body_res
    pure (prestms, poststms, valpat, val', body')
  where
    usedInBody =
      mconcat $ map (`lookupAliases` scope) $ namesToList $ freeIn body <> freeIn form
    resmap = zip (bodyResult body) $ patIdents pat

    mkMerges ::
      (MonadFreshNames m, Buildable rep) =>
      [LoopResultSummary (als, Type)] ->
      m ([(Param DeclType, SubExp)], [Stm rep], [Stm rep])
    mkMerges summaries = do
      ((origmerge, extramerge), (prestms, poststms)) <-
        runWriterT $ partitionEithers <$> mapM mkMerge summaries
      pure (origmerge ++ extramerge, prestms, poststms)

    mkMerge summary
      | Just (update, mergename, mergedec) <- relatedUpdate summary = do
          source <- newVName "modified_source"
          precopy <- newVName $ baseString (updateValue update) <> "_precopy"
          let source_t = snd $ updateType update
              elm_t = source_t `setArrayDims` sliceDims (updateIndices update)
          tell
            ( [ mkLet [Ident source source_t] . BasicOp
                  $ Update
                    Unsafe
                    (updateSource update)
                    (fullSlice source_t $ unSlice $ updateIndices update)
                  $ snd
                  $ mergeParam summary
              ],
              [ mkLet [Ident precopy elm_t] . BasicOp $
                  Index
                    (updateName update)
                    (fullSlice source_t $ unSlice $ updateIndices update),
                mkLet [Ident (updateValue update) elm_t] $ BasicOp $ Replicate mempty $ Var precopy
              ]
            )
          pure $
            Right
              ( Param mempty mergename (toDecl (typeOf mergedec) Unique),
                Var source
              )
      | otherwise = pure $ Left $ mergeParam summary

    mkResAndPat summaries =
      let (origpat, extrapat) = partitionEithers $ map mkResAndPat' summaries
       in origpat ++ extrapat

    mkResAndPat' summary
      | Just (update, _, _) <- relatedUpdate summary =
          Right (Ident (updateName update) (snd $ updateType update))
      | otherwise =
          Left (inPatAs summary)

summariseLoop ::
  ( Aliased rep,
    MonadFreshNames m
  ) =>
  Scope rep ->
  [DesiredUpdate (als, Type)] ->
  Names ->
  [(SubExpRes, Ident)] ->
  [(Param DeclType, SubExp)] ->
  Maybe (m [LoopResultSummary (als, Type)])
summariseLoop scope updates usedInBody resmap merge =
  sequence <$> zipWithM summariseLoopResult resmap merge
  where
    summariseLoopResult (se, v) (fparam, mergeinit)
      | Just update <- find (updateHasValue $ identName v) updates =
          -- Safety condition (7)
          if usedInBody `namesIntersect` lookupAliases (updateSource update) scope
            then Nothing
            else
              if hasLoopInvariantShape fparam
                then Just $ do
                  lowered_array <- newVName "lowered_array"
                  pure
                    LoopResultSummary
                      { resultSubExp = se,
                        inPatAs = v,
                        mergeParam = (fparam, mergeinit),
                        relatedUpdate =
                          Just
                            ( update,
                              lowered_array,
                              updateType update
                            )
                      }
                else Nothing
    summariseLoopResult _ _ =
      Nothing -- XXX: conservative; but this entire pass is going away.
    hasLoopInvariantShape = all loopInvariant . arrayDims . paramType

    merge_param_names = map (paramName . fst) merge

    loopInvariant (Var v) = v `notElem` merge_param_names
    loopInvariant Constant {} = True

data LoopResultSummary dec = LoopResultSummary
  { resultSubExp :: SubExpRes,
    inPatAs :: Ident,
    mergeParam :: (Param DeclType, SubExp),
    relatedUpdate :: Maybe (DesiredUpdate dec, VName, dec)
  }
  deriving (Show)

indexSubstitutions :: (Typed dec) => [LoopResultSummary dec] -> IndexSubstitutions
indexSubstitutions = mapMaybe getSubstitution
  where
    getSubstitution res = do
      (DesiredUpdate _ _ cs _ is _, nm, dec) <- relatedUpdate res
      let name = paramName $ fst $ mergeParam res
      pure (name, (cs, nm, typeOf dec, is))

manipulateResult ::
  (Buildable rep, MonadFreshNames m) =>
  [LoopResultSummary (LetDec rep)] ->
  IndexSubstitutions ->
  m (Result, Stms rep)
manipulateResult summaries substs = do
  let (orig_ses, updated_ses) = partitionEithers $ map unchangedRes summaries
  (subst_ses, res_stms) <- runWriterT $ zipWithM substRes updated_ses substs
  pure (orig_ses ++ subst_ses, stmsFromList res_stms)
  where
    unchangedRes summary =
      case relatedUpdate summary of
        Nothing -> Left $ resultSubExp summary
        Just _ -> Right $ resultSubExp summary
    substRes (SubExpRes res_cs (Var res_v)) (subst_v, (_, nm, _, _))
      | res_v == subst_v =
          pure $ SubExpRes res_cs $ Var nm
    substRes (SubExpRes res_cs res_se) (_, (cs, nm, dec, Slice is)) = do
      v' <- newIdent' (++ "_updated") $ Ident nm $ typeOf dec
      tell
        [ certify (res_cs <> cs) . mkLet [v'] . BasicOp $
            Update Unsafe nm (fullSlice (typeOf dec) is) res_se
        ]
      pure $ varRes $ identName v'
