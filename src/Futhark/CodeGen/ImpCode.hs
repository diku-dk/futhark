-- | Inspired by the paper "Defunctionalizing Push Arrays".
module Futhark.CodeGen.ImpCode
  ( Program
  , Function (..)
  , Param (..)
  , DimSize (..)
  , Type (..)
  , typeRank
  , Code (..)
  , Exp (..)
  , Value (..)
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

data DimSize = ConstSize Int
             | VarSize VName
               deriving (Show)

data Type = Type { typeBasic :: BasicType
                 , typeDims :: [DimSize]
                 }
            deriving (Show)

typeRank :: Type -> Int
typeRank (Type _ shape) = length shape

ppType :: Type -> String
ppType (Type bt [])    = map toLower $ show bt
ppType (Type bt (_:rest)) = "[" ++ ppType (Type bt rest) ++ "]"

data Param = Param { paramName :: VName
                   , paramType :: Type
                   }
             deriving (Show)

type Program a = [(Name, Function a)]

data Function a = Function [Param] [Param] (Code a)
                  deriving (Show)

data Code a = Skip
            | Code a :>>: Code a
            | For VName Exp (Code a)
            | Declare VName BasicType [DimSize]
            | Allocate VName
            | Write VName [Exp] Exp
            | Call [VName] Name [Exp]
            | If Exp (Code a) (Code a)
            | Assert Exp SrcLoc
            | Op a
            deriving (Show)

data Exp = Constant Value
         | BinOp BinOp Exp Exp
         | UnOp UnOp Exp
         | Read VName [Exp]
         | Copy VName
           deriving (Show)

data Value = ArrayVal [Int] (A.Array Int BasicValue) BasicType
           | BasicVal BasicValue
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
entryPointInput params =
  -- We assume that every parameter that is a dimension of another
  -- parameter is not read as input.  This is a hack until we get a
  -- better idea of what we need.
  map paramType $ filter ((`notElem` shapes) . paramName) params
  where shapes = concatMap (mapMaybe isVar . typeDims . paramType) params
        isVar (VarSize v)    = Just v
        isVar (ConstSize {}) = Nothing
