{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Inspecing and modifying t'Pattern's, function parameters and
-- pattern elements.
module Futhark.IR.Prop.Patterns
  ( -- * Function parameters
    paramIdent,
    paramType,
    paramDeclType,

    -- * Pattern elements
    patElemIdent,
    patElemType,
    setPatElemDec,
    patternElements,
    patternIdents,
    patternNames,
    patternTypes,
    patternSize,

    -- * Pattern construction
    basicPattern,
  )
where

import Futhark.IR.Prop.Types (DeclTyped (..), Typed (..))
import Futhark.IR.Syntax

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

-- | The type of a name bound by a t'PatElem'.
patElemType :: Typed dec => PatElemT dec -> Type
patElemType = typeOf

-- | Set the rep of a t'PatElem'.
setPatElemDec :: PatElemT oldattr -> newattr -> PatElemT newattr
setPatElemDec pe x = fmap (const x) pe

-- | Return a list of the 'Ident's bound by the t'Pattern'.
patternIdents :: Typed dec => PatternT dec -> [Ident]
patternIdents = map patElemIdent . patternElements

-- | Return a list of the 'Name's bound by the t'Pattern'.
patternNames :: PatternT dec -> [VName]
patternNames = map patElemName . patternElements

-- | Return a list of the typess bound by the pattern.
patternTypes :: Typed dec => PatternT dec -> [Type]
patternTypes = map identType . patternIdents

-- | Return the number of names bound by the pattern.
patternSize :: PatternT dec -> Int
patternSize (Pattern xs) = length xs

-- | Create a pattern using 'Type' as the attribute.
basicPattern :: [Ident] -> PatternT Type
basicPattern values =
  Pattern $ map patElem values
  where
    patElem (Ident name t) = PatElem name t
