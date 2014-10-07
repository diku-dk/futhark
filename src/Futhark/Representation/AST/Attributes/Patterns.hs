module Futhark.Representation.AST.Attributes.Patterns
       (
         bindeeName
       , bindeeType
       , patternIdents
       , patternNames
       , patternTypes
       , patternSize
       , splitPattern
       )
       where

import Futhark.Representation.AST.Syntax

-- | Return the name bound by a 'Bindee'.
bindeeName :: Bindee lore -> VName
bindeeName = identName . bindeeIdent

-- | Return the type of the name bound by a 'Bindee'.
bindeeType :: Bindee lore -> Type
bindeeType = identType . bindeeIdent

-- | Return a list of the 'Ident's bound by the 'Pattern'.
patternIdents :: Pattern lore -> [Ident]
patternIdents (Pattern xs) = map bindeeIdent xs

-- | Return a list of the 'Name's bound by the 'Pattern'.
patternNames :: Pattern lore -> [VName]
patternNames = map identName . patternIdents

-- | Return a list of the 'types's bound by the 'Pattern'.
patternTypes :: Pattern lore -> [Type]
patternTypes = map identType . patternIdents

-- | Return the number of names bound by the 'Pattern'.
patternSize :: Pattern lore -> Int
patternSize = length . patternBindees

-- | Split given pattern into a list of single-bindee patterns.  Do
-- not do this if the pattern has an existential context.
splitPattern :: Pattern lore -> [Pattern lore]
splitPattern = map (Pattern . (:[])) . patternBindees
