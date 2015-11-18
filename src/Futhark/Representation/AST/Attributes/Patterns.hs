-- | Inspecing and modifying 'Pattern's, function parameters and
-- pattern elements.
module Futhark.Representation.AST.Attributes.Patterns
       (
         -- * Function parameters
         paramIdent
       , paramName
       , paramType
       , paramLore
         -- * Pattern elements
       , patElemIdent
       , patElemName
       , patElemType
       , patElemRequires
       , patElemLore
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
import Futhark.Representation.AST.Attributes.Types (stripArray, setUniqueness)

-- | The name of the 'Ident' bound by an 'FParam'.
paramName :: ParamT attr -> VName
paramName = identName . paramIdent

-- | The type of the 'Ident' bound by an 'FParam'.
paramType :: ParamT attr -> Type
paramType = identType . paramIdent

-- | The name of the ident bound by a 'PatElem'.
patElemName :: PatElemT attr -> VName
patElemName = identName . patElemIdent

-- | The type of a name bound by a 'PatElem'.
patElemType :: PatElemT attr -> Type
patElemType = identType . patElemIdent

-- | The type of the value being bound by a 'PatElem'.
patElemRequires :: PatElemT attr -> Type
patElemRequires (PatElem ident bindage _) =
  bindageRequires (identType ident) bindage

-- | The type of the value being bound by an pattern element with the
-- given result 'Type' and 'Bindage'.
bindageRequires :: Type -> Bindage -> Type
bindageRequires t BindVar =
  t
bindageRequires t (BindInPlace _ _ is) =
  stripArray (length is) t `setUniqueness` Nonunique

-- | Set the lore of a 'PatElem'.
setPatElemLore :: PatElemT oldattr -> newattr -> PatElemT newattr
setPatElemLore (PatElem ident bindage _) =
  PatElem ident bindage

-- | All pattern elements in the pattern - context first, then values.
patternElements :: Pattern lore -> [PatElem lore]
patternElements pat = patternContextElements pat ++ patternValueElements pat

-- | Return a list of the 'Ident's bound by the 'Pattern'.
patternIdents :: Pattern lore -> [Ident]
patternIdents pat = patternContextIdents pat ++ patternValueIdents pat

-- | Return a list of the context 'Ident's bound by the 'Pattern'.
patternContextIdents :: Pattern lore -> [Ident]
patternContextIdents = map patElemIdent . patternContextElements

-- | Return a list of the value 'Ident's bound by the 'Pattern'.
patternValueIdents :: Pattern lore -> [Ident]
patternValueIdents = map patElemIdent . patternValueElements

-- | Return a list of the 'Name's bound by the 'Pattern'.
patternNames :: Pattern lore -> [VName]
patternNames = map identName . patternIdents

-- | Return a list of the 'Name's bound by the context part of the 'Pattern'.
patternContextNames :: Pattern lore -> [VName]
patternContextNames = map identName . patternContextIdents

-- | Return a list of the 'Name's bound by the value part of the 'Pattern'.
patternValueNames :: Pattern lore -> [VName]
patternValueNames = map identName . patternValueIdents

-- | Return a list of the 'types's bound by the 'Pattern'.
patternTypes :: Pattern lore -> [Type]
patternTypes = map identType . patternIdents

-- | Return a list of the 'types's bound by the value part of the 'Pattern'.
patternValueTypes :: Pattern lore -> [Type]
patternValueTypes = map identType . patternValueIdents

-- | Return the number of names bound by the 'Pattern'.
patternSize :: Pattern lore -> Int
patternSize (Pattern context values) = length context + length values

kernelInputName :: KernelInput lore -> VName
kernelInputName = paramName . kernelInputParam

kernelInputType :: KernelInput lore -> Type
kernelInputType = paramType . kernelInputParam

kernelInputIdent :: KernelInput lore -> Ident
kernelInputIdent = paramIdent . kernelInputParam
