module Futhark.Analysis.SymbolTable
  ( SymbolTable (bindings)
  , empty
    -- * Entries
  , Entry
  , deepen
  , bindingDepth
  , valueRange
  , loopVariable
  , entryBinding
  , entryLetBoundLore
  , entryFParamLore
  , asScalExp
    -- * Lookup
  , elem
  , lookup
  , lookupExp
  , lookupSubExp
  , lookupScalExp
  , lookupValue
  , lookupVar
    -- * Insertion
  , insertBinding
  , insertFParams
  , insertLParam
  , insertArrayLParam
  , insertLoopVar
    -- * Bounds
  , updateBounds
  , isAtLeast
    -- * Misc
  , enclosingLoopVars
  )
  where

import Prelude hiding (elem, lookup)

import Control.Applicative hiding (empty)
import Control.Monad
import Data.Ord
import Data.Maybe
import Data.List hiding (elem, insert, lookup)
import qualified Data.Set as S
import qualified Data.HashMap.Lazy as HM

import Futhark.Representation.AST hiding (FParam, FParamT (..))
import qualified Futhark.Representation.AST as AST
import qualified Futhark.Representation.AST.Lore as Lore
import Futhark.Analysis.ScalExp
import Futhark.Substitute
import qualified Futhark.Analysis.AlgSimplify as AS

data SymbolTable lore = SymbolTable {
    loopDepth :: Int
  , bindings :: HM.HashMap VName (Entry lore)
  }

empty :: SymbolTable lore
empty = SymbolTable 0 HM.empty

deepen :: SymbolTable lore -> SymbolTable lore
deepen vtable = vtable { loopDepth = loopDepth vtable + 1 }

data Entry lore = LoopVar (LoopVarEntry lore)
                | LetBound (LetBoundEntry lore)
                | FParam (FParamEntry lore)
                | LParam (LParamEntry lore)

data LoopVarEntry lore =
  LoopVarEntry { loopVarRange        :: Range
               , loopVarBindingDepth :: Int
               }

data LetBoundEntry lore =
  LetBoundEntry { letBoundRange        :: Range
                , letBoundLore         :: Lore.LetBound lore
                , letBoundBinding      :: Binding lore
                , letBoundBindingDepth :: Int
                , letBoundScalExp      :: Maybe ScalExp
                , letBoundBindage      :: Bindage
                }

data FParamEntry lore =
  FParamEntry { fparamRange        :: Range
              , fparamLore         :: Lore.FParam lore
              , fparamBindingDepth :: Int
              }

data LParamEntry lore =
  LParamEntry { lparamRange        :: Range
              , lparamBindingDepth :: Int
              }

isVarBound :: Entry lore -> Maybe (LetBoundEntry lore)
isVarBound (LetBound entry)
  | BindVar <- letBoundBindage entry =
    Just entry
isVarBound _ =
  Nothing

asScalExp :: Entry lore -> Maybe ScalExp
asScalExp = letBoundScalExp <=< isVarBound

bindingDepth :: Entry lore -> Int
bindingDepth (LetBound entry) = letBoundBindingDepth entry
bindingDepth (FParam entry) = fparamBindingDepth entry
bindingDepth (LParam entry) = lparamBindingDepth entry
bindingDepth (LoopVar entry) = loopVarBindingDepth entry

setBindingDepth :: Int -> Entry lore -> Entry lore
setBindingDepth d (LetBound entry) =
  LetBound $ entry { letBoundBindingDepth = d }
setBindingDepth d (FParam entry) =
  FParam $ entry { fparamBindingDepth = d }
setBindingDepth d (LParam entry) =
  LParam $ entry { lparamBindingDepth = d }
setBindingDepth d (LoopVar entry) =
  LoopVar $ entry { loopVarBindingDepth = d }

valueRange :: Entry lore -> Range
valueRange (LetBound entry) = letBoundRange entry
valueRange (FParam entry)   = fparamRange entry
valueRange (LParam entry)   = lparamRange entry
valueRange (LoopVar entry)  = loopVarRange entry

setValueRange :: Range -> Entry lore -> Entry lore
setValueRange range (LetBound entry) =
  LetBound $ entry { letBoundRange = range }
setValueRange range (FParam entry) =
  FParam $ entry { fparamRange = range }
setValueRange range (LParam entry) =
  LParam $ entry { lparamRange = range }
setValueRange range (LoopVar entry) =
  LoopVar $ entry { loopVarRange = range }

entryBinding :: Entry lore -> Maybe (Binding lore)
entryBinding (LetBound entry) = Just $ letBoundBinding entry
entryBinding _                = Nothing

entryLetBoundLore :: Entry lore -> Maybe (Lore.LetBound lore)
entryLetBoundLore (LetBound entry) = Just $ letBoundLore entry
entryLetBoundLore _                = Nothing

entryFParamLore :: Entry lore -> Maybe (Lore.FParam lore)
entryFParamLore (FParam entry) = Just $ fparamLore entry
entryFParamLore _              = Nothing

loopVariable :: Entry lore -> Bool
loopVariable (LoopVar _) = True
loopVariable _           = False

asExp :: Entry lore -> Maybe (Exp lore)
asExp = liftM (bindingExp . letBoundBinding) . isVarBound

instance Substitutable lore => Substitute (LetBoundEntry lore) where
  substituteNames substs entry =
    LetBoundEntry {
        letBoundRange = substituteNames substs $ letBoundRange entry
      , letBoundLore = substituteNames substs $ letBoundLore entry
      , letBoundBinding = substituteNames substs $ letBoundBinding entry
      , letBoundScalExp = substituteNames substs $ letBoundScalExp entry
      , letBoundBindingDepth = letBoundBindingDepth entry
      , letBoundBindage = substituteNames substs $ letBoundBindage entry
      }

instance Substitutable lore => Substitute (FParamEntry lore) where
  substituteNames substs entry =
    FParamEntry {
          fparamRange = substituteNames substs $ fparamRange entry
        , fparamLore = substituteNames substs $ fparamLore entry
        , fparamBindingDepth = fparamBindingDepth entry
      }

instance Substitutable lore => Substitute (LParamEntry lore) where
  substituteNames substs entry =
    LParamEntry {
          lparamRange = substituteNames substs $ lparamRange entry
        , lparamBindingDepth = lparamBindingDepth entry
      }

instance Substitutable lore => Substitute (LoopVarEntry lore) where
  substituteNames substs entry =
    LoopVarEntry {
          loopVarRange = substituteNames substs $ loopVarRange entry
        , loopVarBindingDepth = loopVarBindingDepth entry
      }

instance Substitutable lore =>
         Substitute (Entry lore) where
  substituteNames substs (LetBound entry) =
    LetBound $ substituteNames substs entry
  substituteNames substs (FParam entry) =
    FParam $ substituteNames substs entry
  substituteNames substs (LParam entry) =
    LParam $ substituteNames substs entry
  substituteNames substs (LoopVar entry) =
    LoopVar $ substituteNames substs entry

type Range = (Maybe ScalExp, Maybe ScalExp)

elem :: VName -> SymbolTable lore -> Bool
elem name = isJust . lookup name

lookup :: VName -> SymbolTable lore -> Maybe (Entry lore)
lookup name = HM.lookup name . bindings

lookupExp :: VName -> SymbolTable lore -> Maybe (Exp lore)
lookupExp name vtable = asExp =<< lookup name vtable

lookupSubExp :: VName -> SymbolTable lore -> Maybe SubExp
lookupSubExp name vtable = do
  e <- lookupExp name vtable
  case e of
    PrimOp (SubExp se) -> Just se
    _                  -> Nothing

lookupScalExp :: VName -> SymbolTable lore -> Maybe ScalExp
lookupScalExp name vtable = asScalExp =<< lookup name vtable

lookupValue :: VName -> SymbolTable lore -> Maybe Value
lookupValue name vtable = case lookupSubExp name vtable of
                            Just (Constant val) -> Just $ BasicVal val
                            _                   -> Nothing

lookupVar :: VName -> SymbolTable lore -> Maybe VName
lookupVar name vtable = case lookupSubExp name vtable of
                          Just (Var v) -> Just $ identName v
                          _            -> Nothing

lookupRange :: VName -> SymbolTable lore -> Range
lookupRange name vtable =
  maybe (Nothing, Nothing) valueRange $ lookup name vtable

enclosingLoopVars :: [VName] -> SymbolTable lore -> [VName]
enclosingLoopVars free vtable =
  map fst $
  sortBy (flip (comparing (bindingDepth . snd))) $
  filter (loopVariable . snd) $ mapMaybe fetch free
  where fetch name = do e <- lookup name vtable
                        return (name, e)

defBndEntry :: SymbolTable lore
            -> PatElem lore
            -> Binding lore
            -> LetBoundEntry lore
defBndEntry vtable patElem bnd =
  LetBoundEntry {
      letBoundRange = (Nothing, Nothing)
    , letBoundLore = patElemLore patElem
    , letBoundBinding = bnd
    , letBoundScalExp = toScalExp (`lookupScalExp` vtable) (bindingExp bnd)
    , letBoundBindingDepth = 0
    , letBoundBindage = patElemBindage patElem
    }

bindingEntries :: Binding lore
               -> SymbolTable lore
               -> [LetBoundEntry lore]
-- First, handle single-name bindings.  These are the most common.
bindingEntries bnd@(Let (Pattern [bindee]) _ e) vtable = [entry]
  where entry = (defBndEntry vtable bindee bnd) {
          letBoundRange = range
          }
        range = case e of
          PrimOp (SubExp se) ->
            subExpRange se vtable
          PrimOp (Iota n) ->
            (Just zero, Just $ subExpToScalExp n `SMinus` one)
          PrimOp (Replicate _ v) ->
            subExpRange v vtable
          PrimOp (Rearrange _ _ v) ->
            identRange v vtable
          PrimOp (Split _ _ v) ->
            identRange v vtable
          PrimOp (Copy se) ->
            subExpRange se vtable
          PrimOp (Index _ v _) ->
            identRange v vtable
          _ -> (Nothing, Nothing)
        zero = Val $ IntVal 0
        one = Val $ IntVal 1
-- Then, handle others.  For now, this is only filter.
bindingEntries bnd@(Let (Pattern ps) _ (PrimOp (Partition _ n _ arr))) vtable =
  let (sizes, arrs) = splitAt n ps
  in [ defBndEntry vtable x bnd | x <- sizes ] ++
     map makeBnd arrs
  where makeBnd bindee =
          (defBndEntry vtable bindee bnd) {
            letBoundRange = lookupRange (identName arr) vtable
            }
bindingEntries bnd@(Let pat _ _) vtable =
  map (flip (defBndEntry vtable) bnd) $ patternElements pat

subExpRange :: SubExp -> SymbolTable lore -> Range
subExpRange (Var v) vtable =
  identRange v vtable
subExpRange (Constant bv) _ =
  (Just $ Val bv, Just $ Val bv)

identRange :: Ident -> SymbolTable lore -> Range
identRange = lookupRange . identName

insertEntry :: VName -> Entry lore -> SymbolTable lore
            -> SymbolTable lore
insertEntry name entry =
  insertEntries [(name,entry)]

insertEntries :: [(VName, Entry lore)] -> SymbolTable lore
              -> SymbolTable lore
insertEntries entries vtable =
  vtable { bindings = foldl insertWithDepth (bindings vtable) entries
         }
  where insertWithDepth bnds (name, entry) =
          let entry' = setBindingDepth (loopDepth vtable) entry
          in HM.insert name entry' bnds

insertBinding :: Binding lore -> SymbolTable lore -> SymbolTable lore
insertBinding bnd vtable =
  insertEntries (zip names $ map LetBound $ bindingEntries bnd vtable) vtable
  where names = patternNames $ bindingPattern bnd

insertFParam :: AST.FParam lore
             -> SymbolTable lore
             -> SymbolTable lore
insertFParam fparam = insertEntry name entry
  where name = fparamName fparam
        entry = FParam FParamEntry { fparamRange = (Nothing, Nothing)
                                   , fparamLore = AST.fparamLore fparam
                                   , fparamBindingDepth = 0
                                   }

insertFParams :: [AST.FParam lore] -> SymbolTable lore
              -> SymbolTable lore
insertFParams fparams symtable = foldr insertFParam symtable fparams

insertLParamWithRange :: Param -> Range -> SymbolTable lore
                     -> SymbolTable lore
insertLParamWithRange param range vtable =
  -- We know that the sizes in the type of param are at least zero,
  -- since they are array sizes.
  let vtable' = insertEntry name bind vtable
  in foldr (`isAtLeast` 0) vtable' sizevars
  where bind = LParam LParamEntry { lparamRange = range
                                  , lparamBindingDepth = 0
                                  }
        name = identName param
        sizevars = mapMaybe isVar $ arrayDims $ identType param
        isVar (Var v) = Just $ identName v
        isVar _       = Nothing

insertLParam :: Param -> SymbolTable lore -> SymbolTable lore
insertLParam param =
  insertLParamWithRange param (Nothing, Nothing)

insertArrayLParam :: Param -> Maybe Ident -> SymbolTable lore
                  -> SymbolTable lore
insertArrayLParam param (Just array) vtable =
  -- We now know that the outer size of 'array' is at least one, and
  -- that the inner sizes are at least zero, since they are array
  -- sizes.
  let vtable' = insertLParamWithRange param (identRange array vtable) vtable
  in case arrayDims $ identType array of
    Var v:_ -> (identName v `isAtLeast` 1) vtable'
    _       -> vtable'
insertArrayLParam param Nothing vtable =
  -- Well, we still know that it's a param...
  insertLParam param vtable

insertLoopVar :: VName -> SubExp -> SymbolTable lore -> SymbolTable lore
insertLoopVar name bound vtable = insertEntry name bind vtable
  where bind = LoopVar LoopVarEntry {
            loopVarRange = (Just (Val (IntVal 0)),
                            minus1 <$> toScalExp look (PrimOp $ SubExp bound))
          , loopVarBindingDepth = 0
          }
        look = (`lookupScalExp` vtable)
        minus1 = (`SMinus` Val (IntVal 1))

updateBounds :: Bool -> SubExp -> SymbolTable lore -> SymbolTable lore
updateBounds isTrue cond vtable =
  case toScalExp (`lookupScalExp` vtable) $ PrimOp $ SubExp cond of
    Nothing    -> vtable
    Just cond' ->
      let cond'' | isTrue    = cond'
                 | otherwise = SNot cond'
      in updateBounds' cond'' vtable

-- | Refines the ranges in the symbol table with
--     ranges extracted from branch conditions.
--   `cond' is the condition of the if-branch.
updateBounds' :: ScalExp -> SymbolTable lore -> SymbolTable lore
updateBounds' cond sym_tab =
  foldr updateBound sym_tab $ mapMaybe solve_leq0 $
  either (const []) getNotFactorsLEQ0 $ AS.simplify (SNot cond) ranges
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

               in  either (const []) (:[]) $ AS.simplify leq0_escal ranges
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
        (a,b) <- either (const Nothing) id $ AS.linFormScalE sym e_scal ranges
        case a of
          Val (IntVal (-1)) -> Just (sym, False, b)
          Val (IntVal 1)    -> do
            mb <- either (const Nothing) Just $ AS.simplify (SMinus (Val (IntVal 0)) b) ranges
            Just (sym, True, mb)
          _ -> Nothing

setUpperBound :: VName -> ScalExp -> SymbolTable lore
              -> SymbolTable lore
setUpperBound name bound vtable =
  vtable { bindings = HM.adjust setUpperBound' name $ bindings vtable }
  where setUpperBound' entry =
          let (oldLowerBound, oldUpperBound) = valueRange entry
          in setValueRange
             (oldLowerBound,
              Just $ maybe bound (MaxMin True . (:[bound])) oldUpperBound)
             entry

setLowerBound :: VName -> ScalExp -> SymbolTable lore -> SymbolTable lore
setLowerBound name bound vtable =
  vtable { bindings = HM.adjust setLowerBound' name $ bindings vtable }
  where setLowerBound' entry =
          let (oldLowerBound, oldUpperBound) = valueRange entry
          in setValueRange
             (Just $ maybe bound (MaxMin False . (:[bound])) oldLowerBound,
              oldUpperBound)
             entry

isAtLeast :: VName -> Int -> SymbolTable lore -> SymbolTable lore
isAtLeast name x =
  setLowerBound name $ Val $ IntVal x
