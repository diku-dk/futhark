-- | Definitions of various semantic objects (*not* the Futhark
-- semantics themselves).
module Language.Futhark.Semantic
  ( ImportName
  , mkInitialImport
  , mkImportFrom
  , includeToFilePath
  , includeToString

  , FileModule(..)
  , Imports

  , Namespace(..)
  , Env(..)
  , TySet
  , FunSig(..)
  , NameMap
  , BoundV(..)
  , Mod(..)
  , TypeBinding(..)
  , MTy(..)
  )
where

import Data.Semigroup ((<>))
import Data.Loc
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Semigroup as Sem
import System.FilePath
import qualified System.FilePath.Posix as Posix

import Language.Futhark
import Futhark.Util (dropLast)

-- | Canonical reference to a Futhark code file.  Does not include the
-- @.fut@ extension.  This is most often a path relative to the
-- current working directory of the compiler.
data ImportName = ImportName Posix.FilePath SrcLoc
  deriving (Eq, Ord, Show)

instance Located ImportName where
  locOf (ImportName _ loc) = locOf loc

-- | Create an import name immediately from a file path specified by
-- the user.
mkInitialImport :: FilePath -> ImportName
mkInitialImport s = ImportName (Posix.normalise $ toPOSIX s) noLoc
  where toPOSIX :: FilePath -> Posix.FilePath
        toPOSIX = Posix.joinPath . splitDirectories

-- | We resolve '..' paths here and assume that no shenanigans are
-- going on with symbolic links.  If there is, too bad.  Don't do
-- that.
mkImportFrom :: ImportName -> String -> SrcLoc -> ImportName
mkImportFrom (ImportName includer _) includee
  | Posix.isAbsolute includee = ImportName $ normalise $ makeRelative "/" includee
  | otherwise = ImportName $ normalise $ joinPath $ includer' ++ includee'
  where (dotdots, includee') = span ("../"==) $ splitPath includee
        includer_parts = init $ splitPath includer
        includer'
          | length dotdots > length includer_parts =
              replicate (length dotdots - length includer_parts) "../"
          | otherwise =
              dropLast (length dotdots) includer_parts

-- | Create a @.fut@ file corresponding to an 'ImportName'.
includeToFilePath :: ImportName -> FilePath
includeToFilePath (ImportName s _) = fromPOSIX $ Posix.normalise s <.> "fut"
  where -- | Some bad operating systems do not use forward slash as
        -- directory separator - this is where we convert Futhark includes
        -- (which always use forward slash) to native paths.
        fromPOSIX :: Posix.FilePath -> FilePath
        fromPOSIX = joinPath . Posix.splitDirectories

-- | Produce a human-readable canonicalized string from an
-- 'ImportName'.
includeToString :: ImportName -> String
includeToString (ImportName s _) = Posix.normalise $ Posix.makeRelative "/" s

-- | The result of type checking some file.  Can be passed to further
-- invocations of the type checker.
data FileModule = FileModule { fileAbs :: TySet -- ^ Abstract types.
                             , fileEnv :: Env
                             , fileProg :: Prog
                             }

-- | A mapping from import names to imports.  The ordering is significant.
type Imports = [(String, FileModule)]

-- | The space inhabited by a name.
data Namespace = Term -- ^ Functions and values.
               | Type
               | Signature
               deriving (Eq, Ord, Show, Enum)

-- | A set of abstract types and where their definition is expected.
type TySet = S.Set (QualName VName)

-- | Representation of a module, which is either a plain environment,
-- or a parametric module ("functor" in SML).
data Mod = ModEnv Env
         | ModFun FunSig
         deriving (Show)

-- | A parametric functor consists of a set of abstract types, the
-- environment of its parameter, and the resulting module type.
data FunSig = FunSig { funSigAbs :: TySet
                     , funSigMod :: Mod
                     , funSigMty :: MTy
                     }
            deriving (Show)

-- | Representation of a module type.
data MTy = MTy { mtyAbs :: TySet
                 -- ^ Abstract types in the module type.
               , mtyMod :: Mod
               }
         deriving (Show)

-- | A binding from a name to its definition as a type.
data TypeBinding = TypeAbbr [TypeParam] StructType
                 deriving (Eq, Show)

-- | Type parameters, list of parameter types (optinally named), and
-- return type.  The type parameters are in scope in both parameter
-- types and the return type.  Non-functional values have only a
-- return type.
data BoundV = BoundV [TypeParam] StructType
                deriving (Show)

type NameMap = M.Map (Namespace, Name) (QualName VName)

-- | Modules produces environment with this representation.
data Env = Env { envVtable :: M.Map VName BoundV
               , envTypeTable :: M.Map VName TypeBinding
               , envSigTable :: M.Map VName MTy
               , envModTable :: M.Map VName Mod
               , envNameMap :: NameMap
               } deriving (Show)

instance Sem.Semigroup Env where
  Env vt1 tt1 st1 mt1 nt1 <> Env vt2 tt2 st2 mt2 nt2 =
    Env (vt1<>vt2) (tt1<>tt2) (st1<>st2) (mt1<>mt2) (nt1<>nt2)

instance Monoid Env where
  mempty = Env mempty mempty mempty mempty mempty
  mappend = (Sem.<>)
