{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module contains a bunch of quick and dirty definitions for
-- making interactive development of @futharkc@ easier.  Don't ever use
-- anything exported from this module in actual production code.
-- There is little (if any) error checking, IO exceptions abound and
-- everything might be idiosyncratic and brittle.  Feel free to add
-- your own nasty hacks.

module Futhark.Dev
  ( name
  , ident
  , tident
{-
  , expr
  , typ
  , value
  , lambda
  , tupleLambda
-}
  , prog
  , fromLeft
  , fromRight
  , fromFile
  , fromJust
  )
where

import Data.IORef
import Data.Loc
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import System.IO.Unsafe

import Language.Futhark.Parser

import Futhark.MonadFreshNames
import Futhark.Representation.Basic
import Futhark.Internalise
-- import Futhark.Futhark
import qualified Futhark.Representation.External.Renamer as E
import qualified Futhark.Representation.External.TypeChecker as E

-- | Return a tagged name based on a string.
name :: String -> VName
name = unsafePerformIO . newIDFromString

-- | Return a new, unique identifier.  Uses 'name'.
ident :: String -> Type -> Ident
ident = Ident . name

-- | Return a new, unique identifier, based on a type declaration of
-- the form @"t name"@, for example @"[int] x"@.  Uses 'name'.
tident :: String -> Ident
tident s = case words s of
             [t,k] -> ident k $ typ t
             _ -> error "Bad ident"

uniqueNameSource :: IORef (NameSource VName)
uniqueNameSource = unsafePerformIO $ newIORef newUniqueNameSource
  where newUniqueNameSource = NameSource $ generator 0 HM.empty
        generator i m s =
          case HM.lookup (baseName s) m of
            Just s' -> (s', NameSource $ generator i m)
            Nothing ->
              let s' = s `setID` i
                  m' = HM.insert (baseName s) s' m
              in (s', NameSource $ generator (i+1) m')
{-
uniqueTag :: (NameSource VName -> f -> (t, NameSource VName)) -> f -> t
uniqueTag f x =
  x `seq` unsafePerformIO $ atomicModifyIORef' uniqueNameSource $ \src ->
    let (x', src') = f src x
    in (src', x')

uniqueTagProg :: TypeBox ty => ProgBase ty Name -> ProgBase ty VName
uniqueTagProg = uniqueTag tagProg'

uniqueTagExp :: TypeBox ty => ExpBase ty Name -> ExpBase ty VName
uniqueTagExp = uniqueTag tagExp'

uniqueTagType :: TypeBox ty => ty Name -> ty VName
uniqueTagType = uniqueTag tagType'

uniqueTagLambda :: TypeBox ty => LambdaBase ty Name -> LambdaBase ty VName
uniqueTagLambda = uniqueTag tagLambda'

uniqueTagTupleLambda :: TypeBox ty => TupleLambdaBase ty Name -> TupleLambdaBase ty VName
uniqueTagTupleLambda = uniqueTag tagTupleLambda'

-}

rightResult :: Show a => Either a b -> b
rightResult = either (error . show) id

-- | Parse a string to a program.
prog :: String -> Prog
prog = internaliseProg True . E.tagProg . rightResult . E.checkProg . rightResult . parseFuthark "input"
{-
-- | Parse a string to an expression.
expr :: String -> Exp
expr = uniqueTagExp . rightResult . checkClosedExp . rightResult . parseExp "input"
-}

-- | Parse a string to a type.
typ :: String -> Type
typ "int" = Basic Int
typ "real" = Basic Real
typ "bool" = Basic Bool
typ _ = error "Cannot handle that type yet"

{-

-- | Parse a string to a type.
typ :: String -> Type
typ = uniqueTagType . (`setAliases` HS.empty) . rightResult . parseType "input"

-- | Parse a string to a value.
value :: String -> Value
value = rightResult . parseValue "input"

-- | Parse a string to an anonymous function.  Does not handle curried functions.
lambda :: String -> Lambda
lambda = uniqueTagLambda . rightResult . checkClosedLambda . rightResult . parseLambda "input"
  where checkClosedLambda (AnonymFun params body rettype loc) = do
          body' <- checkOpenExp env body
          return $ AnonymFun params body' rettype loc
            where env = HM.fromList [ (identName param, fromDecl $ identType param)
                                     | param <- params ]
        checkClosedLambda (CurryFun {}) = error "Curries not handled"

tupleLambda :: String -> TupleLambda
tupleLambda = uniqueTagTupleLambda . rightResult . checkClosedTupleLambda . rightResult . parseTupleLambda "input"
  where checkClosedTupleLambda (TupleLambda params body rettype loc) = do
          body' <- checkOpenExp env body
          return $ TupleLambda params body' rettype loc
            where env = HM.fromList [ (identName param, fromDecl $ identType param)
                                     | param <- params ]
-}
-- | Return the 'Left' component of an 'Either' value.
fromLeft :: Either a b -> a
fromLeft (Left x)  = x
fromLeft (Right _) = error "fromLeft: passed Right value."

-- | Return the 'Right' component of an 'Either' value.
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _)  = error "fromRight: passed Left value."

-- | Return the contents of the given file - useful if you don't want
-- to type long strings at the REPL.
fromFile :: FilePath -> String
fromFile = unsafePerformIO . readFile

instance MonadFreshNames IO where
  getNameSource = readIORef uniqueNameSource
  putNameSource = writeIORef uniqueNameSource
