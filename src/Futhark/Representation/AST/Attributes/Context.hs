{-# LANGUAGE FlexibleContexts #-}
module Futhark.Representation.AST.Attributes.Context
  ( expExtContext
  , ifExtContext
  )
  where

import Control.Monad
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Monoid

import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.Attributes.TypeOf
import Futhark.Representation.AST.Attributes.Names
import Futhark.Representation.AST.Syntax

expExtContext :: (Annotations lore, HasScope lore m, Monad m) =>
                 Pattern lore -> Exp lore -> m [Maybe SubExp]
expExtContext pat (If _ tbranch fbranch _) =
  map sameOrNothing <$> ifExtContext pat tbranch fbranch
  where sameOrNothing (Just (x,y)) | x == y = Just x
        sameOrNothing _                     = Nothing
expExtContext pat _ =
  return $ replicate (length $ patternContextElements pat) Nothing

ifExtContext :: (Annotations lore, HasScope lore m, Monad m) =>
                Pattern lore -> Body lore -> Body lore -> m [Maybe (SubExp,SubExp)]
ifExtContext pat tbranch fbranch = do
  ttype <- bodyExtType tbranch
  ftype <- bodyExtType fbranch
  let extdims_t = map (shapeDims . arrayShape) ttype
      extdims_f = map (shapeDims . arrayShape) ftype
      ext_mapping_t :: M.Map VName ExtDimSize
      ext_mapping_t = shapeMapping' (patternValueTypes pat) extdims_t
      ext_mapping_f :: M.Map VName ExtDimSize
      ext_mapping_f = shapeMapping' (patternValueTypes pat) extdims_f
      hasFreeDim name = do
        Free tv <- M.lookup name ext_mapping_t
        Free fv <- M.lookup name ext_mapping_f
        guard $ S.null $ S.intersection (freeIn tv) bound_in_branches
        guard $ S.null $ S.intersection (freeIn fv) bound_in_branches
        return (tv, fv)
  return $ map (hasFreeDim . identName) $ patternContextIdents pat
  where bound_in_branches = boundInBody tbranch <> boundInBody fbranch
