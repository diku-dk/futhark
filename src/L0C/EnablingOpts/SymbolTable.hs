module L0C.EnablingOpts.SymbolTable
  ( SymbolTable
  , Entry (asExp)
  , lookupExp
  , lookupScalExp
  , lookupValue
  , lookupVar
  , insert
  , lookup
  , CtOrId(..)
  )
  where

import Prelude hiding (lookup)

import Control.Applicative

import qualified Data.HashMap.Lazy as HM

import L0C.InternalRep
import L0C.EnablingOpts.ScalExp (ScalExp)
import qualified L0C.EnablingOpts.ScalExp as SE

type SymbolTable = HM.HashMap VName Entry

data Entry = Entry {
    asExp :: Exp
  , asScalExp :: Maybe ScalExp
  }

data CtOrId  = Value Value
             -- ^ A plain value for constant propagation.

             | VarId VName Type
             -- ^ Variable id for copy propagation

             | SymExp Exp
             -- ^ Various other opportunities for simplification.
               deriving (Show)

lookup :: VName -> SymbolTable -> Maybe CtOrId
lookup name vtable = do
  e <- lookupExp name vtable
  case e of
    SubExps [Constant val _] _ -> Just $ Value val
    SubExps [Var v] _          -> Just $ VarId (identName v) (identType v)
    _                          -> Just $ SymExp e

lookupExp :: VName -> SymbolTable -> Maybe Exp
lookupExp name vtable = asExp <$> HM.lookup name vtable

lookupScalExp :: VName -> SymbolTable -> Maybe ScalExp
lookupScalExp name vtable = asScalExp =<< HM.lookup name vtable

lookupValue :: VName -> SymbolTable -> Maybe Value
lookupValue name vtable = case lookupExp name vtable of
                            Just (SubExps [Constant val _] _) -> Just val
                            _                                 -> Nothing

lookupVar :: VName -> SymbolTable -> Maybe VName
lookupVar name vtable = case lookupExp name vtable of
                          Just (SubExps [Var v] _) -> Just $ identName v
                          _                        -> Nothing

insert :: VName -> Exp -> SymbolTable -> SymbolTable
insert name e vtable = HM.insert name bind vtable
  where bind = Entry {
                 asExp = e
               , asScalExp = SE.toScalExp (`lookupScalExp` vtable) e
               }
