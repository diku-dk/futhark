module Language.Futhark.Interpreter.FFI.Server.Interface
  ( Entry (..),
    ServerInterface (..),
  )
where

import Data.Map qualified as M
import Futhark.Server qualified as S
import Futhark.Util.BiMap qualified as BM
import Language.Futhark.Interpreter.FFI.Server.TypeLayout (TypeLayout)
import Language.Futhark.Interpreter.FFI.UIDs (EntryUID, TypeUID)

data Entry = Entry [TypeUID] [TypeUID]
  deriving (Eq, Ord, Show)

data ServerInterface = ServerInterface
  { siEntryPoint :: BM.BiMap S.EntryName EntryUID,
    siEntryPointInfo :: M.Map EntryUID Entry,
    siType :: BM.BiMap S.TypeName TypeUID,
    siTypeLayout :: M.Map TypeUID TypeLayout
  }
  deriving (Show)

instance Monoid ServerInterface where
  mempty = ServerInterface mempty mempty mempty mempty

instance Semigroup ServerInterface where
  (<>)
    (ServerInterface en1 e1 tn1 t1)
    (ServerInterface en2 e2 tn2 t2) =
      ServerInterface
        (en1 <> en2)
        (e1 <> e2)
        (tn1 <> tn2)
        (t1 <> t2)
