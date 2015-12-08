{-# LANGUAGE FlexibleContexts #-}
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
       , patternSize
         -- * Bindage
       , bindageRequires
         -- * Kernel inputs
       , kernelInputName
       , kernelInputType
       , kernelInputIdent
       )
       where

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Types
  (stripArray, Typed(..), DeclTyped(..))
import qualified Futhark.Representation.AST.Annotations as Annotations

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
bindageRequires t (BindInPlace _ _ is) =
  stripArray (length is) t

-- | Set the lore of a 'PatElem'.
setPatElemLore :: PatElemT oldattr -> newattr -> PatElemT newattr
setPatElemLore (PatElem ident bindage _) =
  PatElem ident bindage

-- | All pattern elements in the pattern - context first, then values.
patternElements :: Pattern lore -> [PatElem lore]
patternElements pat = patternContextElements pat ++ patternValueElements pat

-- | Return a list of the 'Ident's bound by the 'Pattern'.
patternIdents :: Annotations.Annotations lore => Pattern lore -> [Ident]
patternIdents pat = patternContextIdents pat ++ patternValueIdents pat

-- | Return a list of the context 'Ident's bound by the 'Pattern'.
patternContextIdents :: Annotations.Annotations lore => Pattern lore -> [Ident]
patternContextIdents = map patElemIdent . patternContextElements

-- | Return a list of the value 'Ident's bound by the 'Pattern'.
patternValueIdents :: Annotations.Annotations lore => Pattern lore -> [Ident]
patternValueIdents = map patElemIdent . patternValueElements

-- | Return a list of the 'Name's bound by the 'Pattern'.
patternNames :: Pattern lore -> [VName]
patternNames = map patElemName . patternElements

-- | Return a list of the 'Name's bound by the context part of the 'Pattern'.
patternContextNames :: Pattern lore -> [VName]
patternContextNames = map patElemName . patternContextElements

-- | Return a list of the 'Name's bound by the value part of the 'Pattern'.
patternValueNames :: Pattern lore -> [VName]
patternValueNames = map patElemName . patternValueElements

-- | Return a list of the 'types's bound by the 'Pattern'.
patternTypes :: Annotations.Annotations lore => Pattern lore -> [Type]
patternTypes = map identType . patternIdents

-- | Return a list of the 'types's bound by the value part of the 'Pattern'.
patternValueTypes :: Annotations.Annotations lore => Pattern lore -> [Type]
patternValueTypes = map identType . patternValueIdents

-- | Return the number of names bound by the 'Pattern'.
patternSize :: Pattern lore -> Int
patternSize (Pattern context values) = length context + length values

kernelInputName :: KernelInput lore -> VName
kernelInputName = paramName . kernelInputParam

kernelInputType :: Typed (Annotations.LParam lore) =>
                   KernelInput lore -> Type
kernelInputType = typeOf . kernelInputParam

kernelInputIdent :: Typed (Annotations.LParam lore) =>
                    KernelInput lore -> Ident
kernelInputIdent = paramIdent . kernelInputParam
