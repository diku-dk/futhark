module Futhark.Optimise.InPlaceLowering.LowerIntoBinding
       (
         lowerUpdate
       , DesiredUpdate (..)
       ) where

import Control.Monad
import Control.Monad.Writer
import Data.Loc
import Data.List (find)
import Data.Maybe (mapMaybe)

import Futhark.Representation.AST
import Futhark.Tools
import Futhark.MonadFreshNames

data DesiredUpdate =
  DesiredUpdate { updateBindee :: Ident
                , updateCertificates :: Certificates
                , updateSource :: Ident
                , updateIndices :: [SubExp]
                , updateValue :: Ident
                , updateSrcLoc :: SrcLoc
                }

updateHasValue :: VName -> DesiredUpdate -> Bool
updateHasValue name = (name==) . identName . updateValue

lowerUpdate :: (Bindable lore, MonadFreshNames m) =>
               Binding lore -> [DesiredUpdate] -> Maybe (m [Binding lore])
lowerUpdate (Let pat _ (LoopOp (DoLoop res merge i bound body loc))) updates = do
  canDo <- lowerUpdateIntoLoop updates pat res merge body loc
  Just $ do
    (pat', res', merge', body') <- canDo
    return [mkLet pat' $ LoopOp $ DoLoop res' merge' i bound body' loc]
lowerUpdate
  (Let pat _ (PrimOp (SubExp (Var v))))
  [DesiredUpdate bindee cs src is val loc]
  | patternIdents pat == [src] =
    Just $ return [mkLet [bindee] $
                   PrimOp $ Update cs v is (Var val) loc]
lowerUpdate _ _ =
  Nothing

lowerUpdateIntoLoop :: (Bindable lore, MonadFreshNames m) =>
                       [DesiredUpdate]
                    -> Pattern lore
                    -> [Ident]
                    -> [(FParam lore, SubExp)]
                    -> Body lore
                    -> SrcLoc
                    -> Maybe (m ([Ident],
                                 [Ident],
                                 [(FParam lore, SubExp)],
                                 Body lore))
lowerUpdateIntoLoop updates pat res merge body loc = do
  -- Algorithm:
  --
  --   0) Map each result of the loop body to a corresponding in-place
  --      update, if one exists.
  --
  --   1) Create new merge variables corresponding to the arrays being
  --      updated; extend the pattern and the @res@ list with these.
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
  mk_in_place_map <- zipWithM isInPlaceValue resmap mergeparams
  Just $ do
    in_place_map <- sequence mk_in_place_map
    let in_place_merge = mapMaybe mkMerge in_place_map
        merge' = merge ++ in_place_merge
        res' = res ++ map (bindeeIdent . fst) in_place_merge
        update_pat = mapMaybe inPlaceIdentDest in_place_map
        pat' = patternIdents pat ++ update_pat
    (bodyresult, new_body_bnds) <- manipulateResult loc in_place_map mergeparams
    let body' = mkBody (bodyBindings body++new_body_bnds) bodyresult
    return (pat', res', merge', body')
  where mergeparams = map fst merge
        resmap = loopResultValues
                 (patternIdents pat) (map identName res)
                 (map bindeeName mergeparams) $
                 resultSubExps $ bodyResult body

        hasLoopInvariantShape = all loopInvariant . arrayDims . bindeeType
        loopInvariant (Var v) =
          identName v `notElem` map bindeeName mergeparams
        loopInvariant (Constant {}) =
          True

        isInPlaceValue (se, Just v) fparam
          | Just update <- find (updateHasValue $ identName v) updates =
            if hasLoopInvariantShape fparam then Just $ do
              ident <-
                newIdent "lowered_array" (identType $ updateBindee update) loc
              return (se, Just (update, ident))
            else Nothing
        isInPlaceValue (se, _) _ =
          Just $ return (se, Nothing)

        mkMerge (_, Just (update, mergeident)) =
          Just (Bindee mergeident (), Var $ updateSource update)
        mkMerge _ =
          Nothing

        inPlaceIdentDest = liftM (updateBindee . fst) . snd

manipulateResult :: (Bindable lore, MonadFreshNames m) =>
                    SrcLoc
                 -> [(SubExp, Maybe (DesiredUpdate, Ident))] -> [FParam lore]
                 -> m (Result, [Binding lore])
manipulateResult loc in_place_map mergeparams = do
  (orig_ses, (new_ses, new_body_bnds)) <-
    runWriterT $ zipWithM manipulateResultPart in_place_map mergeparams
  return (Result (orig_ses ++ new_ses) loc, new_body_bnds)

  where
    manipulateResultPart (se, Nothing) _ =
      return se
    manipulateResultPart
      (se, Just (DesiredUpdate bindee cs _ is _ _, mergeident))
      _ = do
        ident <- lift $ newIdent' (<>"_lowered") bindee
        let update_bnd = mkLet [ident] $
                         PrimOp $ Update cs mergeident is se loc
        tell ([Var ident], [update_bnd])
        return se
