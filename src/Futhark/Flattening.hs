{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.Flattening ( flattenProg )
  where

import Control.Monad
import Control.Applicative

import Futhark.Representation.Basic

newtype FlatM a = FlatM (Either Error a)
                deriving (Monad, Applicative, Functor)

data Error = Error String

instance Show Error where
  show (Error msg) = msg

runFlatM :: FlatM a -> Either Error a
runFlatM (FlatM a) = a

flatError :: String -> FlatM a
flatError msg = FlatM $ Left $ Error msg

flattenProg :: Prog -> Either Error Prog
flattenProg _ = runFlatM $ flatError "flattenProg: not implemented"
