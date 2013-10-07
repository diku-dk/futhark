-- | This module contains a bunch of quick and dirty definitions for
-- making interactive development of @l0c@ easier.  Don't ever use
-- anything exported from this module in actual production code.
-- There is little (if any) error checking, IO exceptions abound and
-- everything might be idiosyncratic and brittle.  Feel free to add
-- your own nasty hacks.

module L0C.Dev
  ( name
  , ident
  , tident
  , expr
  , typ
  )
where

import Data.IORef
import Data.Loc
import qualified Data.Set as S
import System.IO.Unsafe

import Language.L0.Parser

import L0C.L0
import L0C.Renamer
import L0C.TypeChecker

counter :: IORef Int
counter = unsafePerformIO $ newIORef 1000000

-- | Return a unique name.  Yes, the type of this function is
-- non-monadic.  Yes, the name will have a unique tag (at least
-- compared to anything else returned from this function).  No, don't
-- think too much about it.
name :: String -> VName
name k = unsafePerformIO $ do
           i <- atomicModifyIORef counter $ \x -> (x+1,x)
           return $ ID (nameFromString k, i)

-- | Return a new, unique identifier.  Uses 'name'.
ident :: String -> Type -> Ident
ident k t = Ident (name k) t noLoc

-- | Return a new, unique identifier, based on a type declaration of
-- the form @"t name"@, for example @"[int] x"@.  Uses 'name'.
tident :: String -> Ident
tident s = case words s of
             [t,k] -> ident k $ typ t
             _ -> error "Bad ident"

rightResult :: Show a => Either a b -> b
rightResult = either (error . show) id

-- | Parse a string to an expression.
expr :: String -> Exp
expr = tagExp . rightResult . checkClosedExp . rightResult . parseExp "input"

-- | Parse a string to a type.
typ :: String -> Type
typ = tagType . (`setAliases` S.empty) . rightResult . parseType "input"
