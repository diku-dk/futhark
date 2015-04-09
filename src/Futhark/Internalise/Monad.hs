{-# LANGUAGE FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving #-}
module Futhark.Internalise.Monad
  ( InternaliseM
  , runInternaliseM
  , throwError
  , ShapeTable
  , FunTable
  , VarSubstitutions
  , InternaliseEnv(..)
  , FunBinding (..)
  , lookupFunction
  )
  where

import Control.Applicative
import Control.Monad.Except hiding (mapM)
import Control.Monad.State  hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.Writer hiding (mapM)

import qualified Data.HashMap.Lazy as HM
import qualified Data.DList as DL
import Data.List

import qualified Futhark.Representation.External as E
import Futhark.Representation.Basic
import Futhark.MonadFreshNames
import Futhark.Tools

import Prelude hiding (mapM)

data FunBinding = FunBinding
                  { internalFun :: ([VName], [Type],
                                    [(SubExp,Type)] -> Maybe ExtRetType)
                  , externalFun :: (E.DeclType, [E.DeclType])
                  }

type ShapeTable = HM.HashMap VName [SubExp]

type FunTable = HM.HashMap Name FunBinding

-- | A mapping from external variable names to the corresponding
-- internalised identifiers.
type VarSubstitutions = HM.HashMap VName [VName]

data InternaliseEnv = InternaliseEnv {
    envSubsts :: VarSubstitutions
  , envFtable :: FunTable
  , envDoBoundsChecks :: Bool
  }

initialFtable :: FunTable
initialFtable = HM.map addBuiltin builtInFunctions
  where addBuiltin (t, paramts) =
          FunBinding
          ([], map Basic paramts,
           const $ Just $ ExtRetType [Basic t])
          (E.Basic t, map E.Basic paramts)

newtype InternaliseM  a = InternaliseM (BinderT Basic
                                        (ReaderT InternaliseEnv
                                         (StateT VNameSource
                                          (Except String)))
                                        a)
  deriving (Functor, Applicative, Monad,
            MonadWriter (DL.DList Binding),
            MonadReader InternaliseEnv,
            MonadState VNameSource,
            MonadError String)

instance MonadFreshNames InternaliseM where
  getNameSource = get
  putNameSource = put

instance HasTypeEnv InternaliseM where
  askTypeEnv = InternaliseM askTypeEnv

instance MonadBinder InternaliseM where
  type Lore InternaliseM = Basic
  mkLetM pat e = InternaliseM $ mkLetM pat e
  mkBodyM bnds res = InternaliseM $ mkBodyM bnds res
  mkLetNamesM pat e = InternaliseM $ mkLetNamesM pat e

  addBinding      = addBindingWriter
  collectBindings = collectBindingsWriter

runInternaliseM :: MonadFreshNames m =>
                   Bool -> FunTable -> InternaliseM a
                -> m (Either String a)
runInternaliseM boundsCheck ftable (InternaliseM m) =
  modifyNameSource $ \src ->
  let onError e                 = (Left e, src)
      onSuccess ((prog,_),src') = (Right prog, src')
  in either onError onSuccess $ runExcept $
     runStateT (runReaderT (runBinderT m) newEnv) src
  where newEnv = InternaliseEnv {
                   envSubsts = HM.empty
                 , envFtable = initialFtable `HM.union` ftable
                 , envDoBoundsChecks = boundsCheck
                 }

lookupFunction :: Name -> InternaliseM FunBinding
lookupFunction fname = do
  fun <- HM.lookup fname <$> asks envFtable
  case fun of Nothing   -> fail $ "Function '" ++ nameToString fname ++ "' not found"
              Just fun' -> return fun'
