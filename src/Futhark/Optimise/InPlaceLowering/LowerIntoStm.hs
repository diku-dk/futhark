{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Optimise.InPlaceLowering.LowerIntoStm
  ( lowerUpdateKernels
  , lowerUpdate
  , LowerUpdate
  , DesiredUpdate (..)
  ) where

import Control.Monad
import Control.Monad.Writer
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Either
import qualified Data.Map as M

import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.Aliases
import Futhark.Representation.Kernels
import Futhark.Construct
import Futhark.Optimise.InPlaceLowering.SubstituteIndices

data DesiredUpdate attr =
  DesiredUpdate { updateName :: VName -- ^ Name of result.
                , updateType :: attr -- ^ Type of result.
                , updateCertificates :: Certificates
                , updateSource :: VName
                , updateIndices :: Slice SubExp
                , updateValue :: VName
                }
  deriving (Show)

instance Functor DesiredUpdate where
  f `fmap` u = u { updateType = f $ updateType u }

updateHasValue :: VName -> DesiredUpdate attr -> Bool
updateHasValue name = (name==) . updateValue

type LowerUpdate lore m = Scope (Aliases lore)
                          -> Stm (Aliases lore)
                          -> [DesiredUpdate (LetAttr (Aliases lore))]
                          -> Maybe (m [Stm (Aliases lore)])

lowerUpdate :: (MonadFreshNames m, Bindable lore,
                LetAttr lore ~ Type, CanBeAliased (Op lore)) => LowerUpdate lore m
lowerUpdate scope (Let pat aux (DoLoop ctx val form body)) updates = do
  canDo <- lowerUpdateIntoLoop scope updates pat ctx val form body
  Just $ do
    (prebnds, postbnds, ctxpat, valpat, ctx', val', body') <- canDo
    return $
      prebnds ++ [certify (stmAuxCerts aux) $
                  mkLet ctxpat valpat $ DoLoop ctx' val' form body'] ++ postbnds
lowerUpdate _
  (Let pat aux (BasicOp (SubExp (Var v))))
  [DesiredUpdate bindee_nm bindee_attr cs src is val]
  | patternNames pat == [src] =
    let is' = fullSlice (typeOf bindee_attr) is
    in Just $
       return [certify (stmAuxCerts aux <> cs) $
               mkLet [] [Ident bindee_nm $ typeOf bindee_attr] $
               BasicOp $ Update v is' $ Var val]
lowerUpdate _ _ _ =
  Nothing

lowerUpdateKernels :: MonadFreshNames m => LowerUpdate Kernels m
lowerUpdateKernels _
  (Let pat aux (Op (SegOp (SegMap lvl space ts kbody)))) updates
  | all ((`elem` patternNames pat) . updateValue) updates = do
      (pat', kbody', poststms) <-
        lowerUpdatesIntoSegMap pat updates space kbody
      let cs = stmAuxCerts aux <> foldMap updateCertificates updates
      Just $ return $
        certify cs (Let pat' aux $ Op $ SegOp $ SegMap lvl space ts kbody') :
        stmsToList poststms
lowerUpdateKernels scope stm updates = lowerUpdate scope stm updates

lowerUpdatesIntoSegMap :: Pattern (Aliases Kernels)
                       -> [DesiredUpdate (LetAttr (Aliases Kernels))]
                       -> SegSpace
                       -> KernelBody (Aliases Kernels)
                       -> Maybe (Pattern (Aliases Kernels),
                                 KernelBody (Aliases Kernels),
                                 Stms (Aliases Kernels))
lowerUpdatesIntoSegMap pat updates kspace kbody = do
  -- The updates are all-or-nothing.  Being more liberal would require
  -- changes to the in-place-lowering pass itself.
  (pes, krets, poststms) <-
    unzip3 <$> zipWithM onRet (patternElements pat) (kernelBodyResult kbody)
  return (Pattern [] pes,
          kbody { kernelBodyResult = krets },
          mconcat poststms)

  where (gtids, dims) = unzip $ unSegSpace kspace

        onRet (PatElem v v_attr) (Returns _ se)
          | Just (DesiredUpdate bindee_nm bindee_attr _cs src is _val) <-
              find ((==v) . updateValue) updates = do

              guard $ sliceDims is == dims

              let ret = WriteReturns (arrayDims $ snd bindee_attr) src
                        [(map Var gtids, se)]

              return (PatElem bindee_nm bindee_attr,
                      ret,
                      oneStm $ mkLet [] [Ident v $ typeOf v_attr] $
                      BasicOp $ Index bindee_nm is)

        onRet pe ret =
          return (pe, ret, mempty)

lowerUpdateIntoLoop :: (Bindable lore, BinderOps lore,
                        Aliased lore, LetAttr lore ~ (als, Type),
                        MonadFreshNames m) =>
                       Scope lore
                    -> [DesiredUpdate (LetAttr lore)]
                    -> Pattern lore
                    -> [(FParam lore, SubExp)]
                    -> [(FParam lore, SubExp)]
                    -> LoopForm lore
                    -> Body lore
                    -> Maybe (m ([Stm lore],
                                 [Stm lore],
                                 [Ident],
                                 [Ident],
                                 [(FParam lore, SubExp)],
                                 [(FParam lore, SubExp)],
                                 Body lore))
lowerUpdateIntoLoop scope updates pat ctx val form body = do
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
    guard $ not $ paramName p `nameIn` als

  mk_in_place_map <- summariseLoop updates usedInBody resmap val

  Just $ do
    in_place_map <- mk_in_place_map
    (val',prebnds,postbnds) <- mkMerges in_place_map
    let (ctxpat,valpat) = mkResAndPat in_place_map
        idxsubsts = indexSubstitutions in_place_map
    (idxsubsts', newbnds) <- substituteIndices idxsubsts $ bodyStms body
    (body_res, res_bnds) <- manipulateResult in_place_map idxsubsts'
    let body' = mkBody (newbnds<>res_bnds) body_res
    return (prebnds, postbnds, ctxpat, valpat, ctx, val', body')
  where usedInBody = mconcat $ map expandAliases $ namesToList $ freeIn body <> freeIn form
        expandAliases v = case M.lookup v scope of
                            Just (LetInfo attr) -> oneName v <> aliasesOf attr
                            _ -> oneName v
        resmap = zip (bodyResult body) $ patternValueIdents pat

        mkMerges :: (MonadFreshNames m, Bindable lore) =>
                    [LoopResultSummary (als, Type)]
                 -> m ([(Param DeclType, SubExp)], [Stm lore], [Stm lore])
        mkMerges summaries = do
          ((origmerge, extramerge), (prebnds, postbnds)) <-
            runWriterT $ partitionEithers <$> mapM mkMerge summaries
          return (origmerge ++ extramerge, prebnds, postbnds)

        mkMerge summary
          | Just (update, mergename, mergeattr) <- relatedUpdate summary = do
            source <- newVName "modified_source"
            let source_t = snd $ updateType update
                elmident = Ident
                           (updateValue update)
                           (source_t `setArrayDims` sliceDims (updateIndices update))
            tell ([mkLet [] [Ident source source_t] $ BasicOp $ Update
                   (updateSource update)
                   (fullSlice source_t $ updateIndices update) $
                   snd $ mergeParam summary],
                  [mkLet [] [elmident] $ BasicOp $ Index
                   (updateName update)
                   (fullSlice source_t $ updateIndices update)])
            return $ Right (Param
                            mergename
                            (toDecl (typeOf mergeattr) Unique),
                            Var source)
          | otherwise = return $ Left $ mergeParam summary

        mkResAndPat summaries =
          let (origpat,extrapat) = partitionEithers $ map mkResAndPat' summaries
          in (patternContextIdents pat,
              origpat ++ extrapat)

        mkResAndPat' summary
          | Just (update, _, _) <- relatedUpdate summary =
              Right (Ident (updateName update) (snd $ updateType update))
          | otherwise =
              Left (inPatternAs summary)

summariseLoop :: MonadFreshNames m =>
                 [DesiredUpdate (als, Type)]
              -> Names
              -> [(SubExp, Ident)]
              -> [(Param DeclType, SubExp)]
              -> Maybe (m [LoopResultSummary (als, Type)])
summariseLoop updates usedInBody resmap merge =
  sequence <$> zipWithM summariseLoopResult resmap merge
  where summariseLoopResult (se, v) (fparam, mergeinit)
          | Just update <- find (updateHasValue $ identName v) updates =
            if updateSource update `nameIn` usedInBody
            then Nothing
            else if hasLoopInvariantShape fparam then Just $ do
              lowered_array <- newVName "lowered_array"
              return LoopResultSummary { resultSubExp = se
                                       , inPatternAs = v
                                       , mergeParam = (fparam, mergeinit)
                                       , relatedUpdate = Just (update,
                                                               lowered_array,
                                                               updateType update)
                                       }
            else Nothing
        summariseLoopResult _ _ =
          Nothing -- XXX: conservative; but this entire pass is going away.

        hasLoopInvariantShape = all loopInvariant . arrayDims . paramType

        merge_param_names = map (paramName . fst) merge

        loopInvariant (Var v)    = v `notElem` merge_param_names
        loopInvariant Constant{} = True

data LoopResultSummary attr =
  LoopResultSummary { resultSubExp :: SubExp
                    , inPatternAs :: Ident
                    , mergeParam :: (Param DeclType, SubExp)
                    , relatedUpdate :: Maybe (DesiredUpdate attr, VName, attr)
                    }
  deriving (Show)

indexSubstitutions :: [LoopResultSummary attr]
                   -> IndexSubstitutions attr
indexSubstitutions = mapMaybe getSubstitution
  where getSubstitution res = do
          (DesiredUpdate _ _ cs _ is _, nm, attr) <- relatedUpdate res
          let name = paramName $ fst $ mergeParam res
          return (name, (cs, nm, attr, is))

manipulateResult :: (Bindable lore, MonadFreshNames m) =>
                    [LoopResultSummary (LetAttr lore)]
                 -> IndexSubstitutions (LetAttr lore)
                 -> m (Result, Stms lore)
manipulateResult summaries substs = do
  let (orig_ses,updated_ses) = partitionEithers $ map unchangedRes summaries
  (subst_ses, res_bnds) <- runWriterT $ zipWithM substRes updated_ses substs
  return (orig_ses ++ subst_ses, stmsFromList res_bnds)
  where
    unchangedRes summary =
      case relatedUpdate summary of
        Nothing -> Left $ resultSubExp summary
        Just _  -> Right $ resultSubExp summary
    substRes (Var res_v) (subst_v, (_, nm, _, _))
      | res_v == subst_v =
        return $ Var nm
    substRes res_se (_, (cs, nm, attr, is)) = do
      v' <- newIdent' (++"_updated") $ Ident nm $ typeOf attr
      tell [certify cs $ mkLet [] [v'] $ BasicOp $
            Update nm (fullSlice (typeOf attr) is) res_se]
      return $ Var $ identName v'
