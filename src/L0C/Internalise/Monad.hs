{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module L0C.Internalise.Monad
  ( InternaliseM
  , runInternaliseM
  , ShapeTable
  , InternaliseEnv(..)
  , Replacement(..)
  , FunBinding
  , lookupFunction
  )
  where

import Control.Applicative
import Control.Monad.State  hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.Writer hiding (mapM)

import qualified Data.HashMap.Lazy as HM
import qualified Data.DList as DL
import Data.List

import qualified L0C.ExternalRep as E
import L0C.InternalRep
import L0C.MonadFreshNames
import L0C.Tools

import Prelude hiding (mapM)

data Replacement = ArraySubst Ident [Ident]
                 | TupleSubst [Ident]
                 | DirectSubst Ident
                   deriving (Show)

-- | A tuple of a return type and a list of argument types.
type FunBinding = (E.DeclType, [E.DeclType])

type ShapeTable = HM.HashMap VName [SubExp]

data InternaliseEnv = InternaliseEnv {
    envSubsts :: HM.HashMap VName Replacement
  , envFtable :: HM.HashMap Name FunBinding
  }

initialFtable :: HM.HashMap Name FunBinding
initialFtable = HM.map addBuiltin builtInFunctions
  where addBuiltin (t, ts) = (E.Elem $ E.Basic t, map (E.Elem . E.Basic) ts)

type InternaliseM =
  WriterT (DL.DList Binding) (ReaderT InternaliseEnv (State VNameSource))

instance MonadFreshNames InternaliseM where
  getNameSource = get
  putNameSource = put

instance MonadBinder InternaliseM where
  addBinding      = addBindingWriter
  collectBindings = collectBindingsWriter

runInternaliseM :: E.Prog -> InternaliseM a -> a
runInternaliseM prog m = fst $ evalState (runReaderT (runWriterT m) newEnv) newState
  where newState = E.newNameSourceForProg prog
        newEnv = InternaliseEnv {
                   envSubsts = HM.empty
                 , envFtable = initialFtable `HM.union` ftable
                 }
        ftable = HM.fromList
                 [ (fname,(rettype, map E.identType params)) |
                   (fname,rettype,params,_,_) <- E.progFunctions prog ]

lookupFunction :: Name -> InternaliseM FunBinding
lookupFunction fname = do
  fun <- HM.lookup fname <$> asks envFtable
  case fun of Nothing   -> fail $ "Function '" ++ nameToString fname ++ "' not found"
              Just fun' -> return fun'
