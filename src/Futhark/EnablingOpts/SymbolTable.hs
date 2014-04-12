module Futhark.EnablingOpts.SymbolTable
  ( SymbolTable (bindings)
  , empty
  , Entry (..)
  , lookupExp
  , lookupScalExp
  , lookupValue
  , lookupVar
  , enclosingLoopVars
  , insert
  , insert'
  , Bounds
  , insertLoopVar
  , updateBounds
  , isAtLeast
  , lookup
  , CtOrId(..)
  )
  where

import Prelude hiding (lookup)

import Control.Applicative hiding (empty)
import Data.Ord
import Data.Maybe
import Data.List hiding (insert, lookup)
import qualified Data.HashMap.Lazy as HM

import Futhark.InternalRep
import Futhark.EnablingOpts.ScalExp

data SymbolTable = SymbolTable {
    curDepth :: Int
  , bindings :: HM.HashMap VName Entry
  } deriving (Eq, Show)

empty :: SymbolTable
empty = SymbolTable 0 HM.empty

data Entry = Entry {
    asExp :: Maybe Exp
  , asScalExp :: Maybe ScalExp
  , bindingDepth :: Int
  , valueRange :: (Maybe ScalExp, Maybe ScalExp)
  , loopVariable :: Bool
  } deriving (Eq, Show)

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

enclosingLoopVars :: [VName] -> SymbolTable -> [VName]
enclosingLoopVars free vtable =
  map fst $ reverse $
  sortBy (comparing (bindingDepth . snd)) $
  filter (loopVariable . snd) $ mapMaybe fetch free
  where fetch name = do e <- HM.lookup name $ bindings vtable
                        return (name, e)

insert :: VName -> Exp -> SymbolTable -> SymbolTable
insert name e vtable =
  vtable { bindings = HM.insert name bind $ bindings vtable
         , curDepth = curDepth vtable + 1
         }
  where bind = Entry {
                 asExp = Just e
               , asScalExp = toScalExp (`lookupScalExp` vtable) e
               , valueRange = (Nothing, Nothing)
               , bindingDepth = curDepth vtable
               , loopVariable = False
               }

insert' :: VName -> SymbolTable -> SymbolTable
insert' name vtable =
  vtable { bindings = HM.insert name bind $ bindings vtable
         , curDepth = curDepth vtable + 1
         }
  where bind = Entry {
                 asExp = Nothing
               , asScalExp = Nothing
               , valueRange = (Nothing, Nothing)
               , bindingDepth = curDepth vtable
               , loopVariable = False
               }

type Bounds = (Maybe SubExp, Maybe SubExp)

insertLoopVar :: VName -> Bounds -> SymbolTable -> SymbolTable
insertLoopVar name (lower,upper) vtable =
  vtable { bindings = HM.insert name bind $ bindings vtable
         , curDepth = curDepth vtable + 1
         }
  where bind = Entry {
                 asExp = Nothing
               , asScalExp = Nothing
               , valueRange = (lower', upper')
               , bindingDepth = curDepth vtable
               , loopVariable = True
               }
        look = (`lookupScalExp` vtable)
        lower' = toScalExp look =<< (subExp <$> lower)
        upper' = toScalExp look =<< (subExp <$> upper)

updateBounds :: Bool -> SubExp -> SymbolTable -> SymbolTable
updateBounds isTrue cond vtable =
  case toScalExp (`lookupScalExp` vtable) $ subExp cond of
    Nothing    -> vtable
    Just cond' ->
      let cond'' | isTrue    = cond'
                 | otherwise = SNot cond'
      in updateBounds' cond'' vtable

updateBounds' :: ScalExp -> SymbolTable -> SymbolTable
updateBounds' (RelExp LTH0 (Id v)) vtable =
  setUpperBound (identName v) (Val $ IntVal (-1)) vtable
updateBounds' (RelExp LEQ0 (Id v)) vtable =
  setUpperBound (identName v) (Val $ IntVal 0) vtable
updateBounds' (RelExp LTH0 (lower `SMinus` Id v)) vtable =
  setLowerBound (identName v) (lower `SPlus` (Val $ IntVal 1)) vtable
updateBounds' (RelExp LEQ0 (lower `SMinus` Id v)) vtable =
  setLowerBound (identName v) lower vtable

-- XXX: The following should probably be handled through some form of
-- simplification.
updateBounds' (RelExp LTH0 (lower `SPlus` (Val (IntVal (-1)) `STimes` Id v))) vtable =
  setLowerBound (identName v) (lower `SPlus` (Val $ IntVal 1)) vtable
updateBounds' (RelExp LEQ0 (lower `SPlus` (Val (IntVal (-1)) `STimes` Id v))) vtable =
  setLowerBound (identName v) lower vtable

-- FIXME: We need more cases here, probably.  Maybe simplify first?
updateBounds' _ vtable = vtable

setUpperBound :: VName -> ScalExp -> SymbolTable -> SymbolTable
setUpperBound name bound vtable =
  vtable { bindings = HM.adjust setUpperBound' name $ bindings vtable }
  where setUpperBound' bind =
          let (oldLowerBound, oldUpperBound) = valueRange bind
          in bind { valueRange =
                      (oldLowerBound,
                       Just $ maybe bound (MaxMin True . (:[bound])) oldUpperBound)
                  }

setLowerBound :: VName -> ScalExp -> SymbolTable -> SymbolTable
setLowerBound name bound vtable =
  vtable { bindings = HM.adjust setLowerBound' name $ bindings vtable }
  where setLowerBound' bind =
          let (oldLowerBound, oldUpperBound) = valueRange bind
          in bind { valueRange =
                      (Just $ maybe bound (MaxMin False . (:[bound])) oldLowerBound,
                       oldUpperBound)
                  }

isAtLeast :: VName -> Int -> SymbolTable -> SymbolTable
isAtLeast name x =
  setLowerBound name $ Val $ IntVal x
