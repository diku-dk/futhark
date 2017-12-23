{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Optimise.InPlaceLowering.LowerIntoStm
       (
         lowerUpdate
       , DesiredUpdate (..)
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Either
import qualified Data.Set as S

import Prelude

import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.AST
import Futhark.Construct
import Futhark.MonadFreshNames
import Futhark.Optimise.InPlaceLowering.SubstituteIndices
import Futhark.Tools (fullSlice)

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

lowerUpdate :: (Bindable lore, BinderOps lore,
                LetAttr lore ~ (als, Type), Aliased lore,
                MonadFreshNames m) =>
               Stm lore -> [DesiredUpdate (LetAttr lore)] -> Maybe (m [Stm lore])
lowerUpdate (Let pat aux (DoLoop ctx val form body)) updates = do
  canDo <- lowerUpdateIntoLoop updates pat ctx val body
  Just $ do
    (prebnds, postbnds, ctxpat, valpat, ctx', val', body') <- canDo
    return $
      prebnds ++ [certify (stmAuxCerts aux) $
                  mkLet' ctxpat valpat $ DoLoop ctx' val' form body'] ++ postbnds
lowerUpdate
  (Let pat aux (BasicOp (SubExp (Var v))))
  [DesiredUpdate bindee_nm bindee_attr cs src is val]
  | patternNames pat == [src] =
    let is' = fullSlice (typeOf bindee_attr) is
    in Just $
       return [certify (stmAuxCerts aux <> cs) $
               mkLet [] [(Ident bindee_nm $ typeOf bindee_attr,
                          BindInPlace v is')] $
               BasicOp $ SubExp $ Var val]
lowerUpdate
  (Let (Pattern [] [PatElem v BindVar v_attr]) aux e)
  [DesiredUpdate bindee_nm bindee_attr cs src is val]
  | v == val =
    let is' = fullSlice (typeOf bindee_attr) is
    in Just $ return [certify (stmAuxCerts aux <> cs) $
                      mkLet [] [(Ident bindee_nm $ typeOf bindee_attr,
                                 BindInPlace src is')] e,
                      mkLet' [] [Ident v $ typeOf v_attr] $ BasicOp $ Index bindee_nm is']
lowerUpdate _ _ =
  Nothing

lowerUpdateIntoLoop :: (Bindable lore, BinderOps lore,
                        Aliased lore, LetAttr lore ~ (als, Type),
                        MonadFreshNames m) =>
                       [DesiredUpdate (LetAttr lore)]
                    -> Pattern lore
                    -> [(FParam lore, SubExp)]
                    -> [(FParam lore, SubExp)]
                    -> Body lore
                    -> Maybe (m ([Stm lore],
                                 [Stm lore],
                                 [Ident],
                                 [Ident],
                                 [(FParam lore, SubExp)],
                                 [(FParam lore, SubExp)],
                                 Body lore))
lowerUpdateIntoLoop updates pat ctx val body = do
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
  where usedInBody = freeInBody body
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
                updpat = [(Ident source source_t,
                           BindInPlace
                           (updateSource update)
                           (fullSlice source_t $ updateIndices update))]
                elmident = Ident (updateValue update) $ rowType source_t
            tell ([mkLet [] updpat $ BasicOp $ SubExp $ snd $ mergeParam summary],
                  [mkLet' [] [elmident] $ BasicOp $ Index
                   (updateName update) (fullSlice (typeOf $ updateType update) $ updateIndices update)])
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
            if updateSource update `S.member` usedInBody
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
        summariseLoopResult (se, patpart) (fparam, mergeinit) =
          Just $ return LoopResultSummary { resultSubExp = se
                                          , inPatternAs = patpart
                                          , mergeParam = (fparam, mergeinit)
                                          , relatedUpdate = Nothing
                                          }

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
      let is' = fullSlice (typeOf attr) is
      tell [certify cs $ mkLet [] [(v', BindInPlace nm is')] $ BasicOp $ SubExp res_se]
      return $ Var $ identName v'
