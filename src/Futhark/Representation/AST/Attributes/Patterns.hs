{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Inspecing and modifying 'Pattern's, function parameters and
-- pattern elements.
module Futhark.Representation.AST.Attributes.Patterns
       (
         -- * Function parameters
         paramIdent
       , paramType
       , paramDeclType
         -- * Pattern elements
       , patElemIdent
       , patElemType
       , setPatElemLore
       , patternElements
       , patternIdents
       , patternContextIdents
       , patternValueIdents
       , patternNames
       , patternValueNames
       , patternContextNames
       , patternTypes
       , patternValueTypes
       , patternExtTypes
       , patternSize
       -- * Pattern construction
       , basicPattern
       )
       where

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Types
  (existentialiseExtTypes, staticShapes, Typed(..), DeclTyped(..))

-- | The 'Type' of a parameter.
paramType :: Typed attr => Param attr -> Type
paramType = typeOf

-- | The 'DeclType' of a parameter.
paramDeclType :: DeclTyped attr => Param attr -> DeclType
paramDeclType = declTypeOf

-- | An 'Ident' corresponding to a parameter.
paramIdent :: Typed attr => Param attr -> Ident
paramIdent param = Ident (paramName param) (typeOf param)

-- | An 'Ident' corresponding to a pattern element.
patElemIdent :: Typed attr => PatElemT attr -> Ident
patElemIdent pelem = Ident (patElemName pelem) (typeOf pelem)

-- | The type of a name bound by a 'PatElem'.
patElemType :: Typed attr => PatElemT attr -> Type
patElemType = typeOf

-- | Set the lore of a 'PatElem'.
setPatElemLore :: PatElemT oldattr -> newattr -> PatElemT newattr
setPatElemLore pe x = fmap (const x) pe

-- | All pattern elements in the pattern - context first, then values.
patternElements :: PatternT attr -> [PatElemT attr]
patternElements pat = patternContextElements pat ++ patternValueElements pat

-- | Return a list of the 'Ident's bound by the 'Pattern'.
patternIdents :: Typed attr => PatternT attr -> [Ident]
patternIdents pat = patternContextIdents pat ++ patternValueIdents pat

-- | Return a list of the context 'Ident's bound by the 'Pattern'.
patternContextIdents :: Typed attr => PatternT attr -> [Ident]
patternContextIdents = map patElemIdent . patternContextElements

-- | Return a list of the value 'Ident's bound by the 'Pattern'.
patternValueIdents :: Typed attr => PatternT attr -> [Ident]
patternValueIdents = map patElemIdent . patternValueElements

-- | Return a list of the 'Name's bound by the 'Pattern'.
patternNames :: PatternT attr -> [VName]
patternNames = map patElemName . patternElements

-- | Return a list of the 'Name's bound by the context part of the 'Pattern'.
patternContextNames :: PatternT attr -> [VName]
patternContextNames = map patElemName . patternContextElements

-- | Return a list of the 'Name's bound by the value part of the 'Pattern'.
patternValueNames :: PatternT attr -> [VName]
patternValueNames = map patElemName . patternValueElements

-- | Return a list of the 'types's bound by the 'Pattern'.
patternTypes :: Typed attr => PatternT attr -> [Type]
patternTypes = map identType . patternIdents

-- | Return a list of the 'Types's bound by the value part of the 'Pattern'.
patternValueTypes :: Typed attr => PatternT attr -> [Type]
patternValueTypes = map identType . patternValueIdents

-- | Return a list of the 'ExtTypes's bound by the value part of the
-- 'Pattern', with existentials where the sizes are part of the
-- context part of the 'Pattern'.
patternExtTypes :: Typed attr => PatternT attr -> [ExtType]
patternExtTypes pat =
  existentialiseExtTypes (patternContextNames pat)
  (staticShapes (patternValueTypes pat))

-- | Return the number of names bound by the 'Pattern'.
patternSize :: PatternT attr -> Int
patternSize (Pattern context values) = length context + length values

-- | Create a pattern using 'Type' as the attribute.
basicPattern :: [Ident] -> [Ident] -> PatternT Type
basicPattern context values =
  Pattern (map patElem context) (map patElem values)
  where patElem (Ident name t) = PatElem name t
