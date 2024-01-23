{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.IR.SyntaxTests (parseString) where

import Data.String
import Data.Text qualified as T
import Futhark.IR.Parse
import Futhark.IR.Syntax

-- There isn't anything to test in this module, but we define some
-- convenience instances.

parseString :: String -> (FilePath -> T.Text -> Either T.Text a) -> String -> a
parseString desc p =
  either (error . T.unpack) id . p ("IsString " <> desc) . T.pack

instance IsString Type where
  fromString = parseString "Type" parseType

instance IsString DeclExtType where
  fromString = parseString "DeclExtType" parseDeclExtType

instance IsString DeclType where
  fromString = parseString "DeclType" parseDeclType

instance IsString VName where
  fromString = parseString "VName" parseVName

instance IsString SubExp where
  fromString = parseString "SubExp" parseSubExp

instance IsString SubExpRes where
  fromString = parseString "SubExpRes" parseSubExpRes
