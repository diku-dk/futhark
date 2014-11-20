-- | Inspired by the paper "Defunctionalizing Push Arrays".
module Futhark.CodeGen.ImpCode
  ( Program (..)
  , Function (..)
  , Param (..)
  , DimSize (..)
  , Type (..)
  , ValueType (..)
  , typeRank
  , Code (..)
  , MemLocation (..)
  , Exp (..)
  , UnOp (..)
  , ppType
  , entryPointInput
  -- * Re-exports from other modules.
  , module Language.Futhark.Core
  )
  where

import qualified Data.Array as A
import Data.Char (toLower)
import Data.Maybe
import Data.Monoid
import Data.Loc

import Language.Futhark.Core

import Text.PrettyPrint.Mainland

data DimSize = ConstSize Int
             | VarSize VName
               deriving (Show)

data ValueType = Type { typeBasic :: BasicType
                      , typeDims :: [DimSize]
                      }
               deriving (Show)

data Type = Value ValueType
          | Mem DimSize
            deriving (Show)

typeRank :: ValueType -> Int
typeRank (Type _ shape) = length shape

ppType :: Type -> String
ppType (Value (Type bt [])) =
  map toLower $ show bt
ppType (Value (Type bt (_:rest))) =
  "[" ++ ppType (Value (Type bt rest)) ++ "]"
ppType (Mem size) =
  "mem"

data Param = Param { paramName :: VName
                   , paramType :: Type
                   }
             deriving (Show)

newtype Program a = Program [(Name, Function a)]

data Function a = Function [Param] [Param] (Code a)
                  deriving (Show)

-- | When an array is declared, this is where it is stored.
data MemLocation = MemLocation
                   VName -- ^ Name of memory block.
                   DimSize -- ^ Offset into block.
                   deriving (Show)

data Code a = Skip
            | Code a :>>: Code a
            | For VName Exp (Code a)
            | DeclareMem VName DimSize
            | DeclareArray VName BasicType [DimSize]
            | DeclareScalar VName BasicType
            | Allocate VName
            | BackArray VName MemLocation
            | Copy VName Exp VName Exp Exp
              -- ^ Destination, offset in destination, source, offset
              -- in source, number of bytes.
            | Free VName
            | Write VName [Exp] Exp
            | Call [VName] Name [Exp]
            | If Exp (Code a) (Code a)
            | Assert Exp SrcLoc
            | Op a
            deriving (Show)

data Exp = Constant BasicValue
         | BinOp BinOp Exp Exp
         | UnOp UnOp Exp
         | Read VName [Exp]
           deriving (Show)

data UnOp = Not
          | Negate
            deriving (Show)

instance Monoid (Code a) where
  mempty = Skip
  Skip `mappend` y    = y
  x    `mappend` Skip = x
  x    `mappend` y    = x :>>: y

-- | Return the types of values to be read from input, if a function
-- taken the given parameters is used as the entry point of the
-- program.
entryPointInput :: [Param] -> [Type]
entryPointInput params = []
{-
  -- We assume that every parameter that is a dimension of another
  -- parameter is not read as input.  This is a hack until we get a
  -- better idea of what we need.
  mapMaybe paramType $ filter ((`notElem` shapes) . paramName) params
  where shapes = concatMap (mapMaybe isVar . typeDims . paramType) params
        isVar (VarSize v)    = Just v
        isVar (ConstSize {}) = Nothing
        paramType p = case paramKind p of
          ValueParam t -> Just t
          MemParam   _ -> Nothing
-}

-- Prettyprinting definitions.

instance Pretty (Program op) where
  ppr (Program funs) = stack $ map ppFun funs
    where ppFun (name, fun) =
            text "Function " <> ppr name <> colon </> indent 2 (ppr fun)

instance Pretty (Function op) where
  ppr (Function outs ins body) =
    text "Outputs:" </> indent 2 ppOutputs </>
    text "Inputs:" </> indent 2 ppInputs </>
    text "Body:" </> indent 2 (ppr body)
    where ppOutputs = stack $ map ppr outs
          ppInputs = stack $ map ppr ins

instance Pretty Param where
  ppr (Param name ptype) =
    ppr ptype <+> ppr name

instance Pretty Type where
  ppr (Value vt) = ppr vt
  ppr (Mem size) = text "memory of" <+> ppr size <+> text "bytes"

instance Pretty ValueType where
  ppr (Type t ds) = foldr f (ppr t) ds
    where f e s = brackets $ s <> comma <> ppr e

instance Pretty DimSize where
  ppr (ConstSize x) = ppr x
  ppr (VarSize v)   = ppr v

instance Pretty (Code op) where
  ppr (Op _) = text "#<foreign operation>"
  ppr Skip   = text "skip"
  ppr (c1 :>>: c2) = ppr c1 </> ppr c2
  ppr (For i limit body) =
    text "for" <+> ppr i <+> langle <+> ppr limit <> colon </>
    indent 2 (ppr body)
  ppr (DeclareMem name size) =
    text "declare" <+> ppr name <+> text "as memory of size" <+> ppr size
  ppr (DeclareArray name bt shape) =
    text "declare" <+> ppr name <+> text "as array of type" <+> ppr (Type bt shape)
  ppr (DeclareScalar name t) =
    text "declare" <+> ppr name <+> text "as scalar of type" <+> ppr t
  ppr (Allocate name) =
    text "allocate" <+> ppr name
  ppr (BackArray arr mem) =
    ppr arr <> text ".memory" <+> text "<-" <+> ppr mem
  ppr (Write name [] val) =
    ppr name <+> text "<-" <+> ppr val
  ppr (Write name is val) =
    ppr name <> brackets (commasep $ map ppr is) <+> text "<-" <+> ppr val
  ppr (Assert e _) =
    text "assert" <> ppr e

instance Pretty Exp where
  ppr (Constant v) = ppr v
  ppr (BinOp op x y) =
    ppr x <+> text (opStr op) <+> ppr y
  ppr (UnOp Not x) =
    text "not" <+> ppr x
  ppr (UnOp Negate x) =
    text "-" <+> ppr x
  ppr (Read v []) =
    ppr v
  ppr (Read v is) =
    ppr v <> brackets (commasep $ map ppr is)

instance Pretty MemLocation where
  ppr (MemLocation name offset) =
    ppr name <+> text "+" <+> ppr offset
