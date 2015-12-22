{-# LANGUAGE FlexibleContexts #-}
module Futhark.Representation.AST.Attributes.Context
  ( expExtContext
  )
  where

import qualified Data.HashMap.Lazy as HM

import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.Attributes.TypeOf
import Futhark.Representation.AST.Syntax

expExtContext :: (Annotations lore, HasScope lore m, Monad m) =>
                 Pattern lore -> Exp lore -> m [Maybe SubExp]
expExtContext pat (If _ tbranch fbranch _) = do
  ttype <- bodyExtType tbranch
  ftype <- bodyExtType fbranch
  let combtype = generaliseExtTypes ttype ftype
      extdims = map (extShapeDims . arrayShape) combtype
      ext_mapping :: HM.HashMap VName ExtDimSize
      ext_mapping = shapeMapping' (patternValueTypes pat) extdims
      hasFreeDim name = case HM.lookup name ext_mapping of
        Just (Free se) -> Just se
        _              -> Nothing
  return $ map (hasFreeDim . identName) $ patternContextIdents pat
expExtContext pat _ =
  return $ replicate (length $ patternContextElements pat) Nothing
