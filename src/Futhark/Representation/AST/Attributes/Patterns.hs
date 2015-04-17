-- | Inspecing and modifying 'Pattern's, function parameters and
-- pattern elements.
module Futhark.Representation.AST.Attributes.Patterns
       (
         -- * Function parameters
         fparamIdent
       , fparamName
       , fparamType
       , fparamLore
         -- * Pattern elements
       , patElemIdent
       , patElemName
       , patElemType
       , patElemRequires
       , patElemLore
       , setPatElemLore
       , patternIdents
       , patternNames
       , patternTypes
       , patternSize
       , splitPattern
         -- * Bindage
       , bindageRequires
       )
       where

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Types (stripArray, setUniqueness)

-- | The name of the 'Ident' bound by an 'FParam'.
fparamName :: FParamT attr -> VName
fparamName = identName . fparamIdent

-- | The type of the 'Ident' bound by an 'FParam'.
fparamType :: FParamT attr -> Type
fparamType = identType . fparamIdent

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

-- | Return a list of the 'Ident's bound by the 'Pattern'.
patternIdents :: Pattern lore -> [Ident]
patternIdents (Pattern xs) = map patElemIdent xs

-- | Return a list of the 'Name's bound by the 'Pattern'.
patternNames :: Pattern lore -> [VName]
patternNames = map identName . patternIdents

-- | Return a list of the 'types's bound by the 'Pattern'.
patternTypes :: Pattern lore -> [Type]
patternTypes = map identType . patternIdents

-- | Return the number of names bound by the 'Pattern'.
patternSize :: Pattern lore -> Int
patternSize = length . patternElements

-- | Split given pattern into a list of single-bindee patterns.  Do
-- not do this if the pattern has an existential context.
splitPattern :: Pattern lore -> [Pattern lore]
splitPattern = map (Pattern . (:[])) . patternElements
