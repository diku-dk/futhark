{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Analysis.SymbolTable
  ( SymbolTable (bindings)
  , empty
  , fromScope
  , castSymbolTable
    -- * Entries
  , Entry
  , deepen
  , bindingDepth
  , valueRange
  , loopVariable
  , entryBinding
  , entryLetBoundAttr
  , entryFParamLore
  , entryType
  , asScalExp
    -- * Lookup
  , elem
  , lookup
  , lookupExp
  , lookupType
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
  , setUpperBound
  , setLowerBound
  , isAtLeast
    -- * Misc
  , enclosingLoopVars
  , rangesRep
  , typeEnv
  )
  where

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Reader
import Data.Ord
import Data.Maybe
import Data.Monoid
import Data.List hiding (elem, insert, lookup)
import qualified Data.Set          as S
import qualified Data.HashSet      as HS
import qualified Data.HashMap.Lazy as HM

import Prelude hiding (elem, lookup)

import Futhark.Representation.AST hiding (FParam, ParamT (..), paramType, lookupType)
import qualified Futhark.Representation.AST as AST
import Futhark.Analysis.ScalExp
import Futhark.Transform.Substitute
import Futhark.Analysis.Rephrase
import qualified Futhark.Analysis.AlgSimplify as AS
import Futhark.Representation.AST.Attributes.Ranges
  (Range, ScalExpRange, Ranged)
import qualified Futhark.Representation.AST.Attributes.Ranges as Ranges

data SymbolTable lore = SymbolTable {
    loopDepth :: Int
  , bindings :: HM.HashMap VName (Entry lore)
  }

instance Monoid (SymbolTable lore) where
  table1 `mappend` table2 =
    SymbolTable { loopDepth = max (loopDepth table1) (loopDepth table2)
                , bindings = bindings table1 `mappend` bindings table2
                }
  mempty = empty

empty :: SymbolTable lore
empty = SymbolTable 0 HM.empty

fromScope :: Annotations lore => Scope lore -> SymbolTable lore
fromScope = HM.foldlWithKey' insertFreeVar' empty
  where insertFreeVar' m k attr = insertFreeVar k attr m

-- | Try to convert a symbol table for one representation into a
-- symbol table for another.  The two symbol tables will have the same
-- keys, but some entries may be diferent (i.e. some expression
-- entries will have been turned into free variable entries).
castSymbolTable :: (SameScope from to,
                    ExpAttr from ~ ExpAttr to,
                    BodyAttr from ~ BodyAttr to,
                    RetType from ~ RetType to) =>
                   SymbolTable from -> SymbolTable to
castSymbolTable = genCastSymbolTable loopVar letBound fParam lParam freeVar
  where loopVar (LoopVarEntry r d) = LoopVar $ LoopVarEntry r d
        letBound e
          | Just e' <- castBinding $ letBoundBinding e =
              LetBound e { letBoundBinding = e'
                         , letBoundAttr = letBoundAttr e
                         }
          | otherwise =
              FreeVar FreeVarEntry { freeVarAttr = LetInfo $ letBoundAttr e
                                   , freeVarBindingDepth = letBoundBindingDepth e
                                   , freeVarRange = letBoundRange e
                                   }
        fParam e = FParam e { fparamAttr = fparamAttr e }
        lParam e = LParam e { lparamAttr = lparamAttr e }
        freeVar e = FreeVar e { freeVarAttr = castNameInfo $ freeVarAttr e }

genCastSymbolTable :: (LoopVarEntry fromlore -> Entry tolore)
                   -> (LetBoundEntry fromlore -> Entry tolore)
                   -> (FParamEntry fromlore -> Entry tolore)
                   -> (LParamEntry fromlore -> Entry tolore)
                   -> (FreeVarEntry fromlore -> Entry tolore)
                   -> SymbolTable fromlore
                   -> SymbolTable tolore
genCastSymbolTable loopVar letBound fParam lParam freeVar (SymbolTable depth entries) =
  SymbolTable depth $ HM.map onEntry entries
  where onEntry (LoopVar entry) = loopVar entry
        onEntry (LetBound entry) = letBound entry
        onEntry (FParam entry) = fParam entry
        onEntry (LParam entry) = lParam entry
        onEntry (FreeVar entry) = freeVar entry

deepen :: SymbolTable lore -> SymbolTable lore
deepen vtable = vtable { loopDepth = loopDepth vtable + 1 }

data Entry lore = LoopVar (LoopVarEntry lore)
                | LetBound (LetBoundEntry lore)
                | FParam (FParamEntry lore)
                | LParam (LParamEntry lore)
                | FreeVar (FreeVarEntry lore)

data LoopVarEntry lore =
  LoopVarEntry { loopVarRange        :: ScalExpRange
               , loopVarBindingDepth :: Int
               }

data LetBoundEntry lore =
  LetBoundEntry { letBoundRange        :: ScalExpRange
                , letBoundAttr         :: LetAttr lore
                , letBoundBinding      :: Binding lore
                , letBoundBindingDepth :: Int
                , letBoundScalExp      :: Maybe ScalExp
                , letBoundBindage      :: Bindage
                }

data FParamEntry lore =
  FParamEntry { fparamRange        :: ScalExpRange
              , fparamAttr         :: FParamAttr lore
              , fparamBindingDepth :: Int
              }

data LParamEntry lore =
  LParamEntry { lparamRange        :: ScalExpRange
              , lparamAttr         :: LParamAttr lore
              , lparamBindingDepth :: Int
              }

data FreeVarEntry lore =
  FreeVarEntry { freeVarAttr         :: NameInfo lore
               , freeVarBindingDepth :: Int
               , freeVarRange        :: ScalExpRange
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
bindingDepth (FreeVar _) = 0

setBindingDepth :: Int -> Entry lore -> Entry lore
setBindingDepth d (LetBound entry) =
  LetBound $ entry { letBoundBindingDepth = d }
setBindingDepth d (FParam entry) =
  FParam $ entry { fparamBindingDepth = d }
setBindingDepth d (LParam entry) =
  LParam $ entry { lparamBindingDepth = d }
setBindingDepth d (LoopVar entry) =
  LoopVar $ entry { loopVarBindingDepth = d }
setBindingDepth d (FreeVar entry) =
  FreeVar $ entry { freeVarBindingDepth = d }

valueRange :: Entry lore -> ScalExpRange
valueRange (LetBound entry) = letBoundRange entry
valueRange (FParam entry)   = fparamRange entry
valueRange (LParam entry)   = lparamRange entry
valueRange (LoopVar entry)  = loopVarRange entry
valueRange (FreeVar entry)  = freeVarRange entry

setValueRange :: ScalExpRange -> Entry lore -> Entry lore
setValueRange range (LetBound entry) =
  LetBound $ entry { letBoundRange = range }
setValueRange range (FParam entry) =
  FParam $ entry { fparamRange = range }
setValueRange range (LParam entry) =
  LParam $ entry { lparamRange = range }
setValueRange range (LoopVar entry) =
  LoopVar $ entry { loopVarRange = range }
setValueRange range (FreeVar entry) =
  FreeVar $ entry { freeVarRange = range }

entryBinding :: Entry lore -> Maybe (Binding lore)
entryBinding (LetBound entry) = Just $ letBoundBinding entry
entryBinding _                = Nothing

entryLetBoundAttr :: Entry lore -> Maybe (LetAttr lore)
entryLetBoundAttr (LetBound entry) = Just $ letBoundAttr entry
entryLetBoundAttr _                = Nothing

entryFParamLore :: Entry lore -> Maybe (FParamAttr lore)
entryFParamLore (FParam entry) = Just $ fparamAttr entry
entryFParamLore _              = Nothing

loopVariable :: Entry lore -> Bool
loopVariable (LoopVar _) = True
loopVariable _           = False

asExp :: Entry lore -> Maybe (Exp lore)
asExp = fmap (bindingExp . letBoundBinding) . isVarBound

entryType :: Annotations lore => Entry lore -> Type
entryType (LetBound entry) = typeOf $ letBoundAttr entry
entryType (LParam entry)   = typeOf $ lparamAttr entry
entryType (FParam entry)   = typeOf $ fparamAttr entry
entryType (LoopVar _)      = Prim int32
entryType (FreeVar entry)  = typeOf $ freeVarAttr entry

instance Substitutable lore => Substitute (LetBoundEntry lore) where
  substituteNames substs entry =
    LetBoundEntry {
        letBoundRange = substituteNames substs $ letBoundRange entry
      , letBoundAttr = substituteNames substs $ letBoundAttr entry
      , letBoundBinding = substituteNames substs $ letBoundBinding entry
      , letBoundScalExp = substituteNames substs $ letBoundScalExp entry
      , letBoundBindingDepth = letBoundBindingDepth entry
      , letBoundBindage = substituteNames substs $ letBoundBindage entry
      }

instance Substitutable lore => Substitute (FParamEntry lore) where
  substituteNames substs entry =
    FParamEntry {
          fparamRange = substituteNames substs $ fparamRange entry
        , fparamAttr = substituteNames substs $ fparamAttr entry
        , fparamBindingDepth = fparamBindingDepth entry
      }

instance Substitutable lore => Substitute (LParamEntry lore) where
  substituteNames substs entry =
    LParamEntry {
          lparamRange = substituteNames substs $ lparamRange entry
        , lparamBindingDepth = lparamBindingDepth entry
        , lparamAttr = substituteNames substs $ lparamAttr entry
      }

instance Substitutable lore => Substitute (LoopVarEntry lore) where
  substituteNames substs entry =
    LoopVarEntry {
          loopVarRange = substituteNames substs $ loopVarRange entry
        , loopVarBindingDepth = loopVarBindingDepth entry
      }

instance Substitute (NameInfo lore) => Substitute (FreeVarEntry lore) where
  substituteNames substs entry =
    FreeVarEntry {
        freeVarRange = substituteNames substs $ freeVarRange entry
      , freeVarAttr = substituteNames substs $ freeVarAttr entry
      , freeVarBindingDepth = freeVarBindingDepth entry
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
  substituteNames substs (FreeVar entry) =
    FreeVar $ substituteNames substs entry

elem :: VName -> SymbolTable lore -> Bool
elem name = isJust . lookup name

lookup :: VName -> SymbolTable lore -> Maybe (Entry lore)
lookup name = HM.lookup name . bindings

lookupExp :: VName -> SymbolTable lore -> Maybe (Exp lore)
lookupExp name vtable = asExp =<< lookup name vtable

lookupType :: Annotations lore => VName -> SymbolTable lore -> Maybe Type
lookupType name vtable = entryType <$> lookup name vtable

lookupSubExp :: VName -> SymbolTable lore -> Maybe SubExp
lookupSubExp name vtable = do
  e <- lookupExp name vtable
  case e of
    BasicOp (SubExp se) -> Just se
    _                  -> Nothing

lookupScalExp :: VName -> SymbolTable lore -> Maybe ScalExp
lookupScalExp name vtable = asScalExp =<< lookup name vtable

lookupValue :: VName -> SymbolTable lore -> Maybe Value
lookupValue name vtable = case lookupSubExp name vtable of
                            Just (Constant val) -> Just $ PrimVal val
                            _                   -> Nothing

lookupVar :: VName -> SymbolTable lore -> Maybe VName
lookupVar name vtable = case lookupSubExp name vtable of
                          Just (Var v) -> Just v
                          _            -> Nothing

lookupRange :: VName -> SymbolTable lore -> ScalExpRange
lookupRange name vtable =
  maybe (Nothing, Nothing) valueRange $ lookup name vtable

enclosingLoopVars :: [VName] -> SymbolTable lore -> [VName]
enclosingLoopVars free vtable =
  map fst $
  sortBy (flip (comparing (bindingDepth . snd))) $
  filter (loopVariable . snd) $ mapMaybe fetch free
  where fetch name = do e <- lookup name vtable
                        return (name, e)

rangesRep :: SymbolTable lore -> AS.RangesRep
rangesRep = HM.filter knownRange . HM.map toRep . bindings
  where toRep entry = (bindingDepth entry, lower, upper)
          where (lower, upper) = valueRange entry
        knownRange (_, lower, upper) = isJust lower || isJust upper

typeEnv :: SymbolTable lore -> Scope lore
typeEnv = HM.map nameType . bindings
  where nameType (LetBound entry) = LetInfo $ letBoundAttr entry
        nameType (LoopVar _) = IndexInfo
        nameType (FParam entry) = FParamInfo $ fparamAttr entry
        nameType (LParam entry) = LParamInfo $ lparamAttr entry
        nameType (FreeVar entry) = freeVarAttr entry

defBndEntry :: Annotations lore =>
               SymbolTable lore
            -> PatElem lore
            -> Range
            -> Binding lore
            -> LetBoundEntry lore
defBndEntry vtable patElem range bnd =
  LetBoundEntry {
      letBoundRange = simplifyRange $ scalExpRange range
    , letBoundAttr = patElemAttr patElem
    , letBoundBinding = bnd
    , letBoundScalExp =
      runReader (toScalExp (`lookupScalExp` vtable) (bindingExp bnd)) types
    , letBoundBindingDepth = 0
    , letBoundBindage = patElemBindage patElem
    }
  where ranges :: AS.RangesRep
        ranges = rangesRep vtable

        types = typeEnv vtable

        scalExpRange :: Range -> ScalExpRange
        scalExpRange (lower, upper) =
          (scalExpBound fst =<< lower,
           scalExpBound snd =<< upper)

        scalExpBound :: (ScalExpRange -> Maybe ScalExp)
                     -> Ranges.KnownBound
                     -> Maybe ScalExp
        scalExpBound pick (Ranges.VarBound v) =
          pick $ lookupRange v vtable
        scalExpBound _ (Ranges.ScalarBound se) =
          Just se
        scalExpBound _ (Ranges.MinimumBound b1 b2) = do
          b1' <- scalExpBound fst b1
          b2' <- scalExpBound fst b2
          return $ MaxMin True [b1', b2']
        scalExpBound _ (Ranges.MaximumBound b1 b2) = do
          b1' <- scalExpBound snd b1
          b2' <- scalExpBound snd b2
          return $ MaxMin False [b1', b2']

        simplifyRange :: ScalExpRange -> ScalExpRange
        simplifyRange (lower, upper) =
          (simplifyBound lower,
           simplifyBound upper)

        simplifyBound (Just se) =
          Just $ AS.simplify se ranges
        simplifyBound Nothing =
          Nothing

bindingEntries :: Ranged lore =>
                  Binding lore -> SymbolTable lore
               -> [LetBoundEntry lore]
bindingEntries bnd@(Let pat _ _) vtable =
  [ defBndEntry vtable pat_elem (Ranges.rangeOf pat_elem) bnd
  | pat_elem <- patternElements pat
  ]

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

insertBinding :: Ranged lore =>
                 Binding lore
              -> SymbolTable lore
              -> SymbolTable lore
insertBinding bnd vtable =
  insertEntries (zip names $ map LetBound $ bindingEntries bnd vtable) vtable
  where names = patternNames $ bindingPattern bnd

insertFParam :: Annotations lore =>
                AST.FParam lore
             -> SymbolTable lore
             -> SymbolTable lore
insertFParam fparam = insertEntry name entry
  where name = AST.paramName fparam
        entry = FParam FParamEntry { fparamRange = (Nothing, Nothing)
                                   , fparamAttr = AST.paramAttr fparam
                                   , fparamBindingDepth = 0
                                   }

insertFParams :: Annotations lore =>
                 [AST.FParam lore] -> SymbolTable lore
              -> SymbolTable lore
insertFParams fparams symtable = foldr insertFParam symtable fparams

insertLParamWithRange :: Annotations lore =>
                         LParam lore -> ScalExpRange -> SymbolTable lore
                      -> SymbolTable lore
insertLParamWithRange param range vtable =
  -- We know that the sizes in the type of param are at least zero,
  -- since they are array sizes.
  let vtable' = insertEntry name bind vtable
  in foldr (`isAtLeast` 0) vtable' sizevars
  where bind = LParam LParamEntry { lparamRange = range
                                  , lparamAttr = AST.paramAttr param
                                  , lparamBindingDepth = 0
                                  }
        name = AST.paramName param
        sizevars = subExpVars $ arrayDims $ AST.paramType param

insertLParam :: Annotations lore =>
                LParam lore -> SymbolTable lore -> SymbolTable lore
insertLParam param =
  insertLParamWithRange param (Nothing, Nothing)

insertArrayLParam :: Annotations lore =>
                     LParam lore -> Maybe VName -> SymbolTable lore
                  -> SymbolTable lore
insertArrayLParam param (Just array) vtable =
  -- We now know that the outer size of 'array' is at least one, and
  -- that the inner sizes are at least zero, since they are array
  -- sizes.
  let vtable' = insertLParamWithRange param (lookupRange array vtable) vtable
  in case arrayDims <$> lookupType array vtable of
    Just (Var v:_) -> (v `isAtLeast` 1) vtable'
    _              -> vtable'
insertArrayLParam param Nothing vtable =
  -- Well, we still know that it's a param...
  insertLParam param vtable

insertLoopVar :: VName -> SubExp -> SymbolTable lore -> SymbolTable lore
insertLoopVar name bound = insertEntry name bind
  where bind = LoopVar LoopVarEntry {
            loopVarRange = (Just 0,
                            Just $ subExpToScalExp bound int32 - 1)
          , loopVarBindingDepth = 0
          }

insertFreeVar :: VName -> NameInfo lore -> SymbolTable lore -> SymbolTable lore
insertFreeVar name attr = insertEntry name entry
  where entry = FreeVar FreeVarEntry {
            freeVarAttr = attr
          , freeVarRange = (Nothing, Nothing)
          , freeVarBindingDepth = 0
          }

updateBounds :: Annotations lore => Bool -> SubExp -> SymbolTable lore -> SymbolTable lore
updateBounds isTrue cond vtable =
  case runReader (toScalExp (`lookupScalExp` vtable) $ BasicOp $ SubExp cond) types of
    Nothing    -> vtable
    Just cond' ->
      let cond'' | isTrue    = cond'
                 | otherwise = SNot cond'
      in updateBounds' cond'' vtable
  where types = typeEnv vtable

-- | Updating the ranges of all symbols whenever we enter a branch is
-- presently too expensive, and disabled here.
noUpdateBounds :: Bool
noUpdateBounds = True

-- | Refines the ranges in the symbol table with
--     ranges extracted from branch conditions.
--   `cond' is the condition of the if-branch.
updateBounds' :: ScalExp -> SymbolTable lore -> SymbolTable lore
updateBounds' _ sym_tab | noUpdateBounds = sym_tab
updateBounds' cond sym_tab =
  foldr updateBound sym_tab $ mapMaybe solve_leq0 $
  getNotFactorsLEQ0 $ AS.simplify (SNot cond) ranges
    where
      updateBound (sym,True ,bound) = setUpperBound sym bound
      updateBound (sym,False,bound) = setLowerBound sym bound

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
          if scalExpType e_scal /= int32 then []
          else let leq0_escal = if rel == LTH0
                                then SMinus 0 e_scal
                                else SMinus 1 e_scal

               in  [AS.simplify leq0_escal ranges]
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
      solve_leq0 :: ScalExp -> Maybe (VName, Bool, ScalExp)
      solve_leq0 e_scal = do
        sym <- pickRefinedSym S.empty e_scal
        (a,b) <- either (const Nothing) id $ AS.linFormScalE sym e_scal ranges
        case a of
          -1 ->
            Just (sym, False, b)
          1  ->
            let mb = AS.simplify (negate b) ranges
            in Just (sym, True, mb)
          _ -> Nothing

      -- When picking a symbols, @sym@ whose bound it is to be refined:
      -- make sure that @sym@ does not belong to the transitive closure
      -- of the symbols apearing in the ranges of all the other symbols
      -- in the sclar expression (themselves included).
      -- If this does not hold, pick another symbol, rinse and repeat.
      pickRefinedSym :: S.Set VName -> ScalExp -> Maybe VName
      pickRefinedSym elsyms0 e_scal = do
        let candidates = freeIn e_scal
            sym0 = AS.pickSymToElim ranges elsyms0 e_scal
        case sym0 of
            Just sy -> let trclsyms = foldl trClSymsInRange HS.empty $ HS.toList $
                                        candidates `HS.difference` HS.singleton sy
                       in  if   HS.member sy trclsyms
                           then pickRefinedSym (S.insert sy elsyms0) e_scal
                           else sym0
            Nothing -> sym0
      -- computes the transitive closure of the symbols appearing
      -- in the ranges of a symbol
      trClSymsInRange :: HS.HashSet VName -> VName -> HS.HashSet VName
      trClSymsInRange cur_syms sym =
        if HS.member sym cur_syms then cur_syms
        else case HM.lookup sym ranges of
               Just (_,lb,ub) -> let sym_bds = concatMap (HS.toList . freeIn) (catMaybes [lb, ub])
                                 in  foldl trClSymsInRange
                                           (HS.insert sym cur_syms)
                                           (HS.toList $ HS.fromList sym_bds)
               Nothing        -> HS.insert sym cur_syms

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
  setLowerBound name $ fromIntegral x
