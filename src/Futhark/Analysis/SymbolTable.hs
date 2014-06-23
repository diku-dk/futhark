module Futhark.Analysis.SymbolTable
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
  , insertParam
  , insertArrayParam
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
import qualified Data.Set as S
import qualified Data.HashMap.Lazy as HM

import Data.Loc
import Futhark.InternalRep
import Futhark.Analysis.ScalExp
import qualified Futhark.Analysis.AlgSimplify as AS

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
  , valueRange :: Range
  , loopVariable :: Bool
  } deriving (Eq, Show)

type Range = (Maybe ScalExp, Maybe ScalExp)

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
    SubExp (Constant val _) -> Just $ Value val
    SubExp (Var v)          -> Just $ VarId (identName v) (identType v)
    _                       -> Just $ SymExp e

lookupExp :: VName -> SymbolTable -> Maybe Exp
lookupExp name vtable = asExp =<< HM.lookup name (bindings vtable)

lookupScalExp :: VName -> SymbolTable -> Maybe ScalExp
lookupScalExp name vtable = asScalExp =<< HM.lookup name (bindings vtable)

lookupValue :: VName -> SymbolTable -> Maybe Value
lookupValue name vtable = case lookupExp name vtable of
                            Just (SubExp (Constant val _)) -> Just val
                            _                              -> Nothing

lookupVar :: VName -> SymbolTable -> Maybe VName
lookupVar name vtable = case lookupExp name vtable of
                          Just (SubExp (Var v)) -> Just $ identName v
                          _                     -> Nothing

lookupRange :: VName -> SymbolTable -> Range
lookupRange name vtable =
  maybe (Nothing, Nothing) valueRange $ HM.lookup name (bindings vtable)

enclosingLoopVars :: [VName] -> SymbolTable -> [VName]
enclosingLoopVars free vtable =
  map fst $ reverse $
  sortBy (comparing (bindingDepth . snd)) $
  filter (loopVariable . snd) $ mapMaybe fetch free
  where fetch name = do e <- HM.lookup name $ bindings vtable
                        return (name, e)

insert :: VName -> Exp -> SymbolTable -> SymbolTable
insert name e vtable = insertEntry name bind vtable
  where bind = Entry {
                 asExp = Just e
               , asScalExp = toScalExp (`lookupScalExp` vtable) e
               , valueRange = range
               , bindingDepth = curDepth vtable
               , loopVariable = False
               }
        range = case e of
          SubExp se ->
            subExpRange se vtable
          Iota n _ ->
            (Just zero, (`SMinus` one) <$> subExpToScalExp n)
          Replicate _ v _ ->
            subExpRange v vtable
          Rearrange _ _ v _ ->
            subExpRange v vtable
          Split _ se _ _ _ ->
            subExpRange se vtable
          Copy se _ ->
            subExpRange se vtable
          Index _ v _ _ ->
            lookupRange (identName v) vtable
          Filter _ _ [Var v] _ ->
            lookupRange (identName v) vtable -- FIXME: Support filters
                                             -- with more outputs.
          _ -> (Nothing, Nothing)
        zero = Val $ IntVal 0
        one = Val $ IntVal 1

subExpRange :: SubExp -> SymbolTable -> Range
subExpRange (Var v) vtable =
  lookupRange (identName v) vtable
subExpRange (Constant (BasicVal bv) _) _ =
  (Just $ Val bv, Just $ Val bv)
subExpRange (Constant (ArrayVal _ _) _) _ =
  (Nothing, Nothing)

subExpToScalExp :: SubExp -> Maybe ScalExp
subExpToScalExp (Var v)                    = Just $ Id v
subExpToScalExp (Constant (BasicVal bv) _) = Just $ Val bv
subExpToScalExp _                          = Nothing

insertEntry :: VName -> Entry -> SymbolTable -> SymbolTable
insertEntry name entry vtable =
  vtable { bindings = HM.insert name entry $ bindings vtable
         , curDepth = curDepth vtable + 1
         }

insert' :: VName -> SymbolTable -> SymbolTable
insert' name vtable = insertEntry name bind vtable
  where bind = Entry {
                 asExp = Nothing
               , asScalExp = Nothing
               , valueRange = (Nothing, Nothing)
               , bindingDepth = curDepth vtable
               , loopVariable = False
               }

insertParamWithRange :: Param -> Range -> SymbolTable -> SymbolTable
insertParamWithRange param range vtable =
  -- We know that the sizes in the type of param are at least zero,
  -- since they are array sizes.
  let vtable' = insertEntry name bind vtable
  in foldr (`isAtLeast` 0) vtable' sizevars
  where bind = Entry {
                 asExp = Nothing
               , asScalExp = Nothing
               , valueRange = range
               , bindingDepth = curDepth vtable
               , loopVariable = False
               }
        name = identName param
        sizevars = mapMaybe isVar $ arrayDims $ identType param
        isVar (Var v) = Just $ identName v
        isVar _       = Nothing

insertParam :: Param -> SymbolTable -> SymbolTable
insertParam param =
  insertParamWithRange param (Nothing, Nothing)

insertArrayParam :: Param -> SubExp -> SymbolTable -> SymbolTable
insertArrayParam param array vtable =
  -- We now know that the outer size of 'array' is at least one, and
  -- that the inner sizes are at least zero, since they are array
  -- sizes.
  let vtable' = insertParamWithRange param (subExpRange array vtable) vtable
  in case arrayDims $ subExpType array of
    Var v:_ -> (identName v `isAtLeast` 1) vtable'
    _       -> vtable'

insertLoopVar :: VName -> SubExp -> SymbolTable -> SymbolTable
insertLoopVar name bound vtable = insertEntry name bind vtable
  where bind = Entry {
                 asExp = Nothing
               , asScalExp = Nothing
               , valueRange = (Just (Val (IntVal 0)),
                               minus1 <$> toScalExp look (SubExp bound))
               , bindingDepth = curDepth vtable
               , loopVariable = True
               }
        look = (`lookupScalExp` vtable)
        minus1 = (`SMinus` Val (IntVal 1))

updateBounds :: Bool -> SubExp -> SymbolTable -> SymbolTable
updateBounds isTrue cond vtable =
  case toScalExp (`lookupScalExp` vtable) $ SubExp cond of
    Nothing    -> vtable
    Just cond' ->
      let cond'' | isTrue    = SNot cond'
                 | otherwise = cond'
      in updateBounds' (srclocOf cond) cond'' vtable

-- | Refines the ranges in the symbol table with
--     ranges extracted from branch conditions.
--   IMPORTANT: the second argument is the negation
--              of the branch condition!
updateBounds' :: SrcLoc -> ScalExp -> SymbolTable -> SymbolTable
updateBounds' loc not_cond sym_tab =
  foldr updateBound sym_tab $ mapMaybe solve_leq0 $
  either (const []) getNotFactorsLEQ0 $ AS.simplify not_cond loc ranges
    where
      updateBound (sym,True ,bound) = setUpperBound (identName sym) bound
      updateBound (sym,False,bound) = setLowerBound (identName sym) bound

      ranges = HM.filter nonEmptyRange $ HM.map toRep $ bindings sym_tab
      toRep entry = (bindingDepth entry, lower, upper)
        where (lower, upper) = valueRange entry
      nonEmptyRange (_, lower, upper) = isJust lower || isJust upper

      -- | Input: a bool exp in DNF form, named `cond'
      --   It gets the terms of the argument,
      --         i.e., cond = c1 || ... || cn
      --   and negates them.
      --   Returns [not c1, ..., not cn], i.e., the factors
      --   of `not cond' in CNF form: not cond = (not c1) && ... && (not cn)
      getNotFactorsLEQ0 :: ScalExp -> [ScalExp]
      getNotFactorsLEQ0 (RelExp rel e_scal) =
          if scalExpType e_scal /= Int then []
          else let leq0_escal = if rel == LTH0
                                then SMinus (Val (IntVal 0)) e_scal
                                else SMinus (Val (IntVal 1)) e_scal

               in  either (const []) (:[]) $ AS.simplify leq0_escal loc ranges
      getNotFactorsLEQ0 (SLogOr  e1 e2) = getNotFactorsLEQ0 e1 ++ getNotFactorsLEQ0 e2
      getNotFactorsLEQ0 _ = []

      -- | Argument is scalar expression `e'.
      --    Implementation finds the symbol defined at
      --    the highest depth in expression `e', call it `i',
      --    and decomposes e = a*i + b.  If `a' and `b' are
      --    free of `i', AND `a == 1 or -1' THEN the upper/lower
      --    bound can be improved. Otherwise Nothing.
      --
      --  Returns: Nothing or
      --  Just (i, a == 1, -a*b), i.e., (symbol, isUpperBound, bound)
      solve_leq0 :: ScalExp -> Maybe (Ident, Bool, ScalExp)
      solve_leq0 e_scal = do
        sym <- AS.pickSymToElim ranges S.empty e_scal
        (a,b) <- either (const Nothing) id $ AS.linFormScalE sym e_scal loc ranges
        case a of
          Val (IntVal (-1)) -> Just (sym, False, b)
          Val (IntVal 1)    -> do
            mb <- either (const Nothing) Just $ AS.simplify (SMinus (Val (IntVal 0)) b) loc ranges
            Just (sym, True, mb)
          _ -> Nothing

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
