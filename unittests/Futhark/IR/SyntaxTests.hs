{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.IR.SyntaxTests () where

import Data.String
import Data.Text qualified as T
import Futhark.IR.Parse
import Futhark.IR.Syntax

-- There isn't anything to test in this module, but we define some
-- convenience instances.

instance IsString DeclExtType where
  fromString =
    either (error . T.unpack) id
      . parseDeclExtType "IsString DeclExtType"
      . T.pack

instance IsString DeclType where
  fromString =
    either (error . T.unpack) id
      . parseDeclType "IsString DeclType"
      . T.pack
