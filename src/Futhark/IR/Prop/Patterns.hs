{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Inspecing and modifying t'Pat's, function parameters and
-- pattern elements.
module Futhark.IR.Prop.Patterns
  ( -- * Function parameters
    paramIdent,
    paramType,
    paramDeclType,

    -- * Pat elements
    patElemIdent,
    patElemType,
    setPatElemDec,
    patElems,
    patIdents,
    patNames,
    patTypes,
    patSize,

    -- * Pat construction
    basicPat,
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

-- | Return a list of the 'Ident's bound by the t'Pat'.
patIdents :: Typed dec => PatT dec -> [Ident]
patIdents = map patElemIdent . patElems

-- | Return a list of the 'Name's bound by the t'Pat'.
patNames :: PatT dec -> [VName]
patNames = map patElemName . patElems

-- | Return a list of the typess bound by the pattern.
patTypes :: Typed dec => PatT dec -> [Type]
patTypes = map identType . patIdents

-- | Return the number of names bound by the pattern.
patSize :: PatT dec -> Int
patSize (Pat xs) = length xs

-- | Create a pattern using 'Type' as the attribute.
basicPat :: [Ident] -> PatT Type
basicPat values =
  Pat $ map patElem values
  where
    patElem (Ident name t) = PatElem name t
