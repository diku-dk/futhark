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
       )
       where

import Futhark.Representation.AST.Syntax
{-
import Futhark.Representation.AST.Attributes.Types (stripArray)
-}

-- | The name of the 'Ident' bound by an 'FParam'.
fparamName :: FParamT attr -> VName
fparamName = identName . fparamIdent

-- | The type of the 'Ident' bound by an 'FParam'.
fparamType :: FParamT attr -> Type
fparamType = identType . fparamIdent

-- | The ident bound by a 'PatElem'.
patElemIdent :: PatElemT attr -> Ident
patElemIdent (BindVar ident _) = ident
{-
patElemIdent (BindInPlace ident _ _ _) = ident
-}

-- | The name of the ident bound by a 'PatElem'.
patElemName :: PatElemT attr -> VName
patElemName = identName . patElemIdent

-- | The type of a name bound by a 'PatElem'.
patElemType :: PatElemT attr -> Type
patElemType = identType . patElemIdent

-- | The type of the value being bound by a 'PatElem'.
patElemRequires :: PatElemT attr -> Type
patElemRequires (BindVar ident _) =
  identType ident
  {-
patElemRequires (BindInPlace _ src is _) =
  stripArray (length is) $ identType src
-}

-- | The lore of a 'PatElem'.
patElemLore :: PatElemT attr -> attr
patElemLore (BindVar _ lore) = lore
{-
patElemLore (BindInPlace _ _ _ lore) = lore
-}

-- | Set the lore of a 'PatElem'.
setPatElemLore :: PatElemT oldattr -> newattr -> PatElemT newattr
setPatElemLore (BindVar ident _) lore =
  BindVar ident lore
  {-
setPatElemLore (BindInPlace ident src is _) lore =
  BindInPlace ident src is lore
-}

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
