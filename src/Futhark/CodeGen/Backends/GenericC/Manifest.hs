{-# LANGUAGE OverloadedStrings #-}

-- | C manifest data structure and serialisation to JSON.
--
-- A manifest contains machine-readable information about the API of
-- the compiled Futhark program.  Specifically which entry points are
-- available, which types are exposed, and what their C names are.
module Futhark.CodeGen.Backends.GenericC.Manifest
  ( Manifest (..),
    Input (..),
    Output (..),
    EntryPoint (..),
    Type (..),
    ArrayOps (..),
    OpaqueOps (..),
    manifestToJSON,
  )
where

import Data.Aeson (ToJSON (..), object)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as JSON
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (bimap)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)

-- | Manifest info for an entry point parameter.
data Input = Input
  { inputName :: T.Text,
    inputType :: T.Text,
    inputUnique :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Manifest info for an entry point return value.
data Output = Output
  { outputType :: T.Text,
    outputUnique :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Manifest info for an entry point.
data EntryPoint = EntryPoint
  { entryPointCFun :: T.Text,
    entryPointOutputs :: [Output],
    entryPointInputs :: [Input]
  }
  deriving (Eq, Ord, Show)

-- | The names of the C functions implementing the operations on some
-- array type.
data ArrayOps = ArrayOps
  { arrayFree :: T.Text,
    arrayShape :: T.Text,
    arrayValues :: T.Text,
    arrayNew :: T.Text
  }
  deriving (Eq, Ord, Show)

-- | The names of the C functions implementing the operations on some
-- opaque type.
data OpaqueOps = OpaqueOps
  { opaqueFree :: T.Text,
    opaqueStore :: T.Text,
    opaqueRestore :: T.Text
  }
  deriving (Eq, Ord, Show)

-- | Manifest info for a non-scalar type.  Scalar types are not part
-- of the manifest for a program.
data Type
  = -- | ctype, Futhark elemtype, rank.
    TypeArray T.Text T.Text Int ArrayOps
  | TypeOpaque T.Text OpaqueOps
  deriving (Eq, Ord, Show)

-- | A manifest for a compiled program.
data Manifest = Manifest
  { -- | A mapping from Futhark entry points to how they are
    -- represented in C.
    manifestEntryPoints :: M.Map T.Text EntryPoint,
    -- | A mapping from Futhark type name to how they are represented
    -- at the C level.  Should not contain any of the primitive scalar
    -- types.  For array types, these have empty dimensions,
    -- e.g. @[]i32@.
    manifestTypes :: M.Map T.Text Type,
    -- | The compiler backend used to
    -- compile the program, e.g. @c@.
    manifestBackend :: T.Text
  }
  deriving (Eq, Ord, Show)

instance JSON.ToJSON ArrayOps where
  toJSON (ArrayOps free shape values new) =
    object
      [ ("free", toJSON free),
        ("shape", toJSON shape),
        ("values", toJSON values),
        ("new", toJSON new)
      ]

instance JSON.ToJSON OpaqueOps where
  toJSON (OpaqueOps free store restore) =
    object
      [ ("free", toJSON free),
        ("store", toJSON store),
        ("restore", toJSON restore)
      ]

instance JSON.ToJSON Manifest where
  toJSON (Manifest entry_points types backend) =
    object
      [ ("backend", toJSON backend),
        ( "entry_points",
          object $ map (bimap JSON.fromText onEntryPoint) $ M.toList entry_points
        ),
        ( "types",
          object $ map (bimap JSON.fromText onType) $ M.toList types
        )
      ]
    where
      onEntryPoint (EntryPoint cfun outputs inputs) =
        object
          [ ("cfun", toJSON cfun),
            ("outputs", toJSON $ map onOutput outputs),
            ("inputs", toJSON $ map onInput inputs)
          ]

      onOutput (Output t u) =
        object
          [ ("type", toJSON t),
            ("unique", toJSON u)
          ]

      onInput (Input p t u) =
        object
          [ ("name", toJSON p),
            ("type", toJSON t),
            ("unique", toJSON u)
          ]

      onType (TypeArray t et rank ops) =
        object
          [ ("kind", "array"),
            ("ctype", toJSON t),
            ("rank", toJSON rank),
            ("elemtype", toJSON et),
            ("ops", toJSON ops)
          ]
      onType (TypeOpaque t ops) =
        object
          [ ("kind", "opaque"),
            ("ctype", toJSON t),
            ("ops", toJSON ops)
          ]

-- | Serialise a manifest in JSON format, so it can be read from other
-- tools.  The schema supposed to be at
-- https://futhark-lang.org/manifest.schema.json.
manifestToJSON :: Manifest -> T.Text
manifestToJSON = toStrict . encodeToLazyText
