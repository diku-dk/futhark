module Language.Futhark.Interpreter.FFI.Server.TypeLayout
  ( TypeLayout (..),
  )
where

import Data.Map qualified as M
import Futhark.Server qualified as S
import Language.Futhark.Interpreter.FFI.UIDs (TypeUID)
import Language.Futhark.Interpreter.FFI.Values (PrimitiveType)
import Prelude hiding (init)

data TypeLayout
  = TLPrimitive PrimitiveType
  | TLArray TypeUID
  | TLRecord [(S.FieldName, TypeUID)]
  | TLSum (M.Map S.VariantName [TypeUID])
  | TLOpaque
  deriving (Show, Eq, Ord)
