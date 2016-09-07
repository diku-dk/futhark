{-# LANGUAGE FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Futhark.Internalise.Monad
  ( InternaliseM
  , runInternaliseM
  , throwError
  , FunTable
  , VarSubstitutions
  , InternaliseEnv(..)
  , FunBinding (..)
  , lookupFunction
  , bindingIdentTypes
  , bindingParamTypes
    -- * Convenient reexports
  , module Futhark.Tools
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

import qualified Language.Futhark as E
import Futhark.Representation.SOACS
import Futhark.MonadFreshNames
import Futhark.Tools

import Prelude hiding (mapM)

data FunBinding = FunBinding
                  { internalFun :: ([VName], [DeclType],
                                    [(SubExp,Type)] -> Maybe ExtRetType)
                  , externalFun :: (E.StructType, [E.StructType])
                  }

type FunTable = HM.HashMap Name FunBinding

-- | A mapping from external variable names to the corresponding
-- internalised subexpressions.
type VarSubstitutions = HM.HashMap VName [SubExp]

data InternaliseEnv = InternaliseEnv {
    envSubsts :: VarSubstitutions
  , envFtable :: FunTable
  , envDoBoundsChecks :: Bool
  }

newtype InternaliseM  a = InternaliseM (BinderT SOACS
                                        (ReaderT InternaliseEnv
                                         (StateT VNameSource
                                          (Except String)))
                                        a)
  deriving (Functor, Applicative, Monad,
            MonadWriter (DL.DList Binding),
            MonadReader InternaliseEnv,
            MonadState VNameSource,
            MonadError String,
            HasScope SOACS,
            LocalScope SOACS)

instance MonadFreshNames InternaliseM where
  getNameSource = get
  putNameSource = put

instance MonadBinder InternaliseM where
  type Lore InternaliseM = SOACS
  mkLetM pat e = InternaliseM $ mkLetM pat e
  mkBodyM bnds res = InternaliseM $ mkBodyM bnds res
  mkLetNamesM pat e = InternaliseM $ mkLetNamesM pat e

  addBinding =
    InternaliseM . addBinding
  collectBindings (InternaliseM m) =
    InternaliseM $ collectBindings m

runInternaliseM :: MonadFreshNames m =>
                   FunTable -> InternaliseM a
                -> m (Either String a)
runInternaliseM ftable (InternaliseM m) =
  modifyNameSource $ \src ->
  let onError e                 = (Left e, src)
      onSuccess ((prog,_),src') = (Right prog, src')
  in either onError onSuccess $ runExcept $
     runStateT (runReaderT (runBinderT m mempty) newEnv) src
  where newEnv = InternaliseEnv {
                   envSubsts = HM.empty
                 , envFtable = ftable
                 , envDoBoundsChecks = True
                 }

lookupFunction :: Name -> InternaliseM FunBinding
lookupFunction fname = do
  fun <- HM.lookup fname <$> asks envFtable
  case fun of Nothing   -> fail $ "Function '" ++ nameToString fname ++ "' not found"
              Just fun' -> return fun'

bindingIdentTypes :: [Ident] -> InternaliseM a
                  -> InternaliseM a
bindingIdentTypes idents (InternaliseM m) =
  InternaliseM $ localScope (typeEnvFromIdents idents) m

typeEnvFromIdents :: [Ident] -> Scope SOACS
typeEnvFromIdents = HM.fromList . map assoc
  where assoc ident = (identName ident, LetInfo $ identType ident)

bindingParamTypes :: [LParam] -> InternaliseM a
                  -> InternaliseM a
bindingParamTypes = bindingIdentTypes . map paramIdent
