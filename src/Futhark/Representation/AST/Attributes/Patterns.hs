{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Inspecing and modifying t'Pattern's, function parameters and
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
       , patternSize
       -- * Pattern construction
       , basicPattern
       )
       where

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Types (Typed(..), DeclTyped(..))

-- | The 'Type' of a parameter.
paramType :: Typed dec => Param dec -> Type
paramType = typeOf

-- | The 'DeclType' of a parameter.
paramDeclType :: DeclTyped dec => Param dec -> DeclType
paramDeclType = declTypeOf

-- | An 'Ident' corresponding to a parameter.
paramIdent :: Typed dec => Param dec -> Ident
paramIdent param = Ident (paramName param) (typeOf param)

-- | An 'Ident' corresponding to a pattern element.
patElemIdent :: Typed dec => PatElemT dec -> Ident
patElemIdent pelem = Ident (patElemName pelem) (typeOf pelem)

-- | The type of a name bound by a 'PatElem'.
patElemType :: Typed dec => PatElemT dec -> Type
patElemType = typeOf

-- | Set the lore of a 'PatElem'.
setPatElemLore :: PatElemT oldattr -> newattr -> PatElemT newattr
setPatElemLore pe x = fmap (const x) pe

-- | All pattern elements in the pattern - context first, then values.
patternElements :: PatternT dec -> [PatElemT dec]
patternElements pat = patternContextElements pat ++ patternValueElements pat

-- | Return a list of the 'Ident's bound by the t'Pattern'.
patternIdents :: Typed dec => PatternT dec -> [Ident]
patternIdents pat = patternContextIdents pat ++ patternValueIdents pat

-- | Return a list of the context 'Ident's bound by the t'Pattern'.
patternContextIdents :: Typed dec => PatternT dec -> [Ident]
patternContextIdents = map patElemIdent . patternContextElements

-- | Return a list of the value 'Ident's bound by the t'Pattern'.
patternValueIdents :: Typed dec => PatternT dec -> [Ident]
patternValueIdents = map patElemIdent . patternValueElements

-- | Return a list of the 'Name's bound by the t'Pattern'.
patternNames :: PatternT dec -> [VName]
patternNames = map patElemName . patternElements

-- | Return a list of the 'Name's bound by the context part of the t'Pattern'.
patternContextNames :: PatternT dec -> [VName]
patternContextNames = map patElemName . patternContextElements

-- | Return a list of the 'Name's bound by the value part of the t'Pattern'.
patternValueNames :: PatternT dec -> [VName]
patternValueNames = map patElemName . patternValueElements

-- | Return a list of the typess bound by the pattern.
patternTypes :: Typed dec => PatternT dec -> [Type]
patternTypes = map identType . patternIdents

-- | Return a list of the typess bound by the value part of the pattern.
patternValueTypes :: Typed dec => PatternT dec -> [Type]
patternValueTypes = map identType . patternValueIdents

-- | Return the number of names bound by the pattern.
patternSize :: PatternT dec -> Int
patternSize (Pattern context values) = length context + length values

-- | Create a pattern using 'Type' as the attribute.
basicPattern :: [Ident] -> [Ident] -> PatternT Type
basicPattern context values =
  Pattern (map patElem context) (map patElem values)
  where patElem (Ident name t) = PatElem name t
