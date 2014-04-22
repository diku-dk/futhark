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
  -- * Re-exports from other modules.
  , module Language.Futhark.Core
  )
  where

import qualified Data.Array as A
import Data.Char (toLower)
import Data.Monoid
import Data.Loc

import Language.Futhark.Core

data DimSize = ConstSize Int
             | VarSize VName
               deriving (Show)

data Type = Type BasicType [DimSize]
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

type Program = [(Name, Function)]

data Function = Function [Param] [Param] Code
                deriving (Show)

data Code = Skip
          | Code :>>: Code
          | For VName Exp Code
          | Declare VName BasicType [DimSize]
          | Allocate VName
          | Write VName [Exp] Exp
          | Call [VName] Name [Exp]
          | If Exp Code Code
          | Assert Exp SrcLoc
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

instance Monoid Code where
  mempty = Skip
  Skip `mappend` y    = y
  x    `mappend` Skip = x
  x    `mappend` y    = x :>>: y
