{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Futhark.Internalise.Monad
  ( InternaliseM
  , runInternaliseM
  , ShapeTable
  , FunTable
  , InternaliseEnv(..)
  , FunBinding (..)
  , lookupFunction
  , withNonuniqueReplacements
  )
  where

import Control.Arrow
import Control.Applicative
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

-- | A tuple of a return type, a list of argument types, and the
-- argument types of the internalised function.
data FunBinding = FunBinding { internalFun :: ([ExtType], [VName], [Type])
                             , externalFun :: (E.DeclType, [E.DeclType])
                             }

type ShapeTable = HM.HashMap VName [SubExp]

type FunTable = HM.HashMap Name FunBinding

data InternaliseEnv = InternaliseEnv {
    envSubsts :: HM.HashMap VName [Ident]
  , envFtable :: FunTable
  , envDoBoundsChecks :: Bool
  }

initialFtable :: FunTable
initialFtable = HM.map addBuiltin builtInFunctions
  where addBuiltin (t, paramts) =
          FunBinding
          ([Basic t], [], map Basic paramts)
          (E.Elem $ E.Basic t, map (E.Elem . E.Basic) paramts)

type InternaliseM =
  WriterT (DL.DList Binding) (ReaderT InternaliseEnv (State VNameSource))

instance MonadFreshNames InternaliseM where
  getNameSource = get
  putNameSource = put

instance BindableM InternaliseM where
  type Lore InternaliseM = Basic
  mkLetM pat e = return $ mkLet pat' e
    where pat' = [ (ident, bindage)
                 | PatElem ident bindage _ <- patternElements pat
                 ]
  mkBodyM bnds res = return $ mkBody bnds res
  mkLetNamesM = mkLetNames

instance MonadBinder InternaliseM where
  addBinding      = addBindingWriter
  collectBindings = collectBindingsWriter

runInternaliseM :: MonadFreshNames m =>
                   Bool -> FunTable -> InternaliseM a -> m a
runInternaliseM boundsCheck ftable m =
  modifyNameSource $
  first fst . runState (runReaderT (runWriterT m) newEnv)
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

withNonuniqueReplacements :: InternaliseM a -> InternaliseM a
withNonuniqueReplacements = local $ \env ->
  env { envSubsts = HM.map (map makeNonunique) $ envSubsts env }
  where makeNonunique = (`setIdentUniqueness` Nonunique)
