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
       , patElemRequires
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
         -- * Bindage
       , bindageRequires
         -- *
       , basicPattern
       , basicPattern'
       )
       where

import qualified Data.Set as S

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Types
  (elemType, arrayOfShape, existentialiseExtTypes, staticShapes,
   Typed(..), DeclTyped(..))

-- | The 'Type' of a parameter.
paramType :: Typed attr => ParamT attr -> Type
paramType = typeOf

-- | The 'DeclType' of a parameter.
paramDeclType :: DeclTyped attr => ParamT attr -> DeclType
paramDeclType = declTypeOf

-- | An 'Ident' corresponding to a parameter.
paramIdent :: Typed attr => ParamT attr -> Ident
paramIdent param = Ident (paramName param) (typeOf param)

-- | An 'Ident' corresponding to a pattern element.
patElemIdent :: Typed attr => PatElemT attr -> Ident
patElemIdent pelem = Ident (patElemName pelem) (typeOf pelem)

-- | The type of a name bound by a 'PatElem'.
patElemType :: Typed attr => PatElemT attr -> Type
patElemType = typeOf

-- | The type of the value being bound by a 'PatElem'.
patElemRequires :: Typed attr => PatElemT attr -> Type
patElemRequires (PatElem _ bindage attr) =
  bindageRequires (typeOf attr) bindage

-- | The type of the value being bound by an pattern element with the
-- given result 'Type' and 'Bindage'.
bindageRequires :: Type -> Bindage -> Type
bindageRequires t BindVar =
  t
bindageRequires t (BindInPlace _ slice) =
  Prim (elemType t) `arrayOfShape` Shape (sliceDims slice)

-- | Set the lore of a 'PatElem'.
setPatElemLore :: PatElemT oldattr -> newattr -> PatElemT newattr
setPatElemLore (PatElem ident bindage _) =
  PatElem ident bindage

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
  existentialiseExtTypes (S.fromList $ patternContextNames pat)
  (staticShapes (patternValueTypes pat))

-- | Return the number of names bound by the 'Pattern'.
patternSize :: PatternT attr -> Int
patternSize (Pattern context values) = length context + length values

-- | Create a pattern using 'Type' as the attribute.
basicPattern :: [(Ident,Bindage)] -> [(Ident,Bindage)] -> PatternT Type
basicPattern context values =
  Pattern (map patElem context) (map patElem values)
  where patElem (Ident name t,bindage) = PatElem name bindage t

-- | Like 'basicPattern', but all 'Bindage's are assumed to be
-- 'BindVar'.
basicPattern' :: [Ident] -> [Ident] -> PatternT Type
basicPattern' context values =
  basicPattern (map addBindVar context) (map addBindVar values)
    where addBindVar name = (name, BindVar)
