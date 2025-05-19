-- | Definitions of various semantic objects (*not* the Futhark
-- semantics themselves).
module Language.Futhark.Semantic
  ( ImportName,
    mkInitialImport,
    mkImportFrom,
    includeToFilePath,
    includeToString,
    includeToText,
    FileModule (..),
    Imports,
    Namespace (..),
    Env (..),
    TySet,
    FunModType (..),
    NameMap,
    BoundV (..),
    Mod (..),
    TypeBinding (..),
    MTy (..),
  )
where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Futhark.Util (fromPOSIX, toPOSIX)
import Futhark.Util.Pretty
import Language.Futhark
import System.FilePath qualified as Native
import System.FilePath.Posix qualified as Posix
import Prelude hiding (mod)

-- | Create an import name immediately from a file path specified by
-- the user.
mkInitialImport :: Native.FilePath -> ImportName
mkInitialImport = ImportName . Posix.normalise . toPOSIX

-- | We resolve '..' paths here and assume that no shenanigans are
-- going on with symbolic links.  If there is, too bad.  Don't do
-- that.
mkImportFrom :: ImportName -> String -> ImportName
mkImportFrom (ImportName includer) includee
  | Posix.isAbsolute includee = ImportName includee
  | otherwise =
      ImportName . Posix.normalise . Posix.joinPath . resolveDotDot [] $
        init (Posix.splitPath includer) ++ Posix.splitPath includee
  where
    resolveDotDot parts [] = reverse parts
    resolveDotDot parts@("../" : _) ("../" : todo) = resolveDotDot ("../" : parts) todo
    resolveDotDot (_ : parts) ("../" : todo) = resolveDotDot parts todo
    resolveDotDot parts (p : todo) = resolveDotDot (p : parts) todo

-- | Create a @.fut@ file corresponding to an 'ImportName'.
includeToFilePath :: ImportName -> Native.FilePath
includeToFilePath (ImportName s) = fromPOSIX $ Posix.normalise s Posix.<.> "fut"

-- | Produce a human-readable canonicalized string from an
-- 'ImportName'.
includeToString :: ImportName -> String
includeToString (ImportName s) = Posix.normalise s

-- | Produce a human-readable canonicalized text from an
-- 'ImportName'.
includeToText :: ImportName -> T.Text
includeToText (ImportName s) = T.pack $ Posix.normalise s

-- | The result of type checking some file.  Can be passed to further
-- invocations of the type checker.
data FileModule = FileModule
  { -- | Abstract types.
    fileAbs :: TySet,
    -- | The environment made available when importing this module.
    fileEnv :: Env,
    fileProg :: Prog,
    -- | The environment at the bottom of the file.  Includes local
    -- parts.
    fileScope :: Env
  }

-- | A mapping from import names to imports.  The ordering is significant.
type Imports = [(ImportName, FileModule)]

-- | The space inhabited by a name.
data Namespace
  = -- | Functions and values.
    Term
  | Type
  | Signature
  deriving (Eq, Ord, Show, Enum)

-- | A mapping of abstract types to their liftedness.
type TySet = M.Map (QualName VName) Liftedness

-- | Representation of a module, which is either a plain environment,
-- or a parametric module ("functor" in SML).
data Mod
  = ModEnv Env
  | ModFun FunModType
  deriving (Show)

-- | A parametric functor consists of a set of abstract types, the
-- environment of its parameter, and the resulting module type.
data FunModType = FunModType
  { funModTypeAbs :: TySet,
    funModTypeMod :: Mod,
    funModTypeMty :: MTy
  }
  deriving (Show)

-- | Representation of a module type.
data MTy = MTy
  { -- | Abstract types in the module type.
    mtyAbs :: TySet,
    mtyMod :: Mod
  }
  deriving (Show)

-- | A binding from a name to its definition as a type.  We allow a
-- return type here to support type abbreviations that hide some inner
-- sizes (these must necessarily be 'Lifted' or 'SizeLifted').
data TypeBinding = TypeAbbr Liftedness [TypeParam] StructRetType
  deriving (Eq, Show)

-- | Type parameters, list of parameter types (optinally named), and
-- return type.  The type parameters are in scope in both parameter
-- types and the return type.  Non-functional values have only a
-- return type.
data BoundV = BoundV
  { boundValTParams :: [TypeParam],
    boundValType :: StructType
  }
  deriving (Show)

-- | A mapping from names (which always exist in some namespace) to a
-- unique (tagged) name.
type NameMap = M.Map (Namespace, Name) (QualName VName)

-- | Modules produces environment with this representation.
data Env = Env
  { envVtable :: M.Map VName BoundV,
    envTypeTable :: M.Map VName TypeBinding,
    envModTypeTable :: M.Map VName MTy,
    envModTable :: M.Map VName Mod,
    envNameMap :: NameMap
  }
  deriving (Show)

instance Semigroup Env where
  Env vt1 tt1 st1 mt1 nt1 <> Env vt2 tt2 st2 mt2 nt2 =
    Env (vt1 <> vt2) (tt1 <> tt2) (st1 <> st2) (mt1 <> mt2) (nt1 <> nt2)

instance Pretty Namespace where
  pretty Term = "name"
  pretty Type = "type"
  pretty Signature = "module type"

instance Monoid Env where
  mempty = Env mempty mempty mempty mempty mempty

instance Pretty MTy where
  pretty = pretty . mtyMod

instance Pretty Mod where
  pretty (ModEnv e) = pretty e
  pretty (ModFun (FunModType _ mod mty)) = pretty mod <+> "->" </> pretty mty

instance Pretty Env where
  pretty (Env vtable ttable sigtable modtable _) =
    nestedBlock "{" "}" $
      stack $
        punctuate line $
          concat
            [ map renderTypeBind (M.toList ttable),
              map renderValBind (M.toList vtable),
              map renderModType (M.toList sigtable),
              map renderMod (M.toList modtable)
            ]
    where
      renderTypeBind (name, TypeAbbr l tps tp) =
        p l
          <+> prettyName name
          <> mconcat (map ((" " <>) . pretty) tps)
          <> " ="
            <+> pretty tp
        where
          p Lifted = "type^"
          p SizeLifted = "type~"
          p Unlifted = "type"
      renderValBind (name, BoundV tps t) =
        "val"
          <+> prettyName name
          <> mconcat (map ((" " <>) . pretty) tps)
            <+> ":"
            <+> pretty t
      renderModType (name, _sig) =
        "module type" <+> prettyName name
      renderMod (name, mod) =
        "module" <+> prettyName name <> " =" <+> pretty mod
