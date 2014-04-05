module L0C.EnablingOpts.SymbolTable
  ( SymbolTable
  , empty
  , filter
  , Entry (asExp)
  , Bounds
  , lookupExp
  , lookupScalExp
  , lookupValue
  , lookupVar
  , insert
  , insertBounded
  , lookup
  , CtOrId(..)
  )
  where

import Prelude hiding (lookup, filter)

import qualified Data.HashMap.Lazy as HM

import L0C.InternalRep
import L0C.EnablingOpts.ScalExp (ScalExp)
import qualified L0C.EnablingOpts.ScalExp as SE

data SymbolTable = SymbolTable {
    curDepth :: Int
  , bindings :: HM.HashMap VName Entry
  }

empty :: SymbolTable
empty = SymbolTable 0 HM.empty

filter :: (Entry -> Bool) -> SymbolTable -> SymbolTable
filter p vtable = vtable { bindings = HM.filter p $ bindings vtable }

type Bounds = (Maybe ScalExp, Maybe ScalExp)

data Entry = Entry {
    asExp :: Maybe Exp
  , asScalExp :: Maybe ScalExp
  , bindingDepth :: Int
  , valueRange :: Bounds
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
lookupExp name vtable = asExp =<< HM.lookup name (bindings vtable)

lookupScalExp :: VName -> SymbolTable -> Maybe ScalExp
lookupScalExp name vtable = asScalExp =<< HM.lookup name (bindings vtable)

lookupValue :: VName -> SymbolTable -> Maybe Value
lookupValue name vtable = case lookupExp name vtable of
                            Just (SubExps [Constant val _] _) -> Just val
                            _                                 -> Nothing

lookupVar :: VName -> SymbolTable -> Maybe VName
lookupVar name vtable = case lookupExp name vtable of
                          Just (SubExps [Var v] _) -> Just $ identName v
                          _                        -> Nothing

insert :: VName -> Exp -> SymbolTable -> SymbolTable
insert name e vtable =
  vtable { bindings = HM.insert name bind $ bindings vtable
         , curDepth = curDepth vtable + 1
         }
  where bind = Entry {
                 asExp = Just e
               , asScalExp = SE.toScalExp (`lookupScalExp` vtable) e
               , valueRange = (Nothing, Nothing)
               , bindingDepth = curDepth vtable
               }

insertBounded :: VName -> Bounds -> SymbolTable -> SymbolTable
insertBounded name bounds vtable =
  vtable { bindings = HM.insert name bind $ bindings vtable
         , curDepth = curDepth vtable + 1
         }
  where bind = Entry {
                 asExp = Nothing
               , asScalExp = Nothing
               , valueRange = bounds
               , bindingDepth = curDepth vtable
               }
