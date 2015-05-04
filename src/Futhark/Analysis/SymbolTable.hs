module Futhark.Analysis.SymbolTable
  ( SymbolTable (bindings)
  , empty
  , fromTypeEnv
    -- * Entries
  , Entry
  , deepen
  , bindingDepth
  , valueRange
  , loopVariable
  , entryBinding
  , entryLetBoundLore
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
import qualified Data.Set as S
import qualified Data.HashMap.Lazy as HM

import Prelude hiding (elem, lookup)

import Futhark.Representation.AST hiding (FParam, ParamT (..), paramType, paramLore, lookupType)
import qualified Futhark.Representation.AST as AST
import qualified Futhark.Representation.AST.Lore as Lore
import Futhark.Analysis.ScalExp
import Futhark.Substitute
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

fromTypeEnv :: TypeEnv -> SymbolTable lore
fromTypeEnv = HM.foldlWithKey' insertFreeVar' empty
  where insertFreeVar' m k v = insertFreeVar k v m

deepen :: SymbolTable lore -> SymbolTable lore
deepen vtable = vtable { loopDepth = loopDepth vtable + 1 }

data Entry lore = LoopVar (LoopVarEntry lore)
                | LetBound (LetBoundEntry lore)
                | FParam (FParamEntry lore)
                | LParam (LParamEntry lore)
                | FreeVar FreeVarEntry

data LoopVarEntry lore =
  LoopVarEntry { loopVarRange        :: ScalExpRange
               , loopVarBindingDepth :: Int
               }

data LetBoundEntry lore =
  LetBoundEntry { letBoundRange        :: ScalExpRange
                , letBoundLore         :: Lore.LetBound lore
                , letBoundBinding      :: Binding lore
                , letBoundBindingDepth :: Int
                , letBoundScalExp      :: Maybe ScalExp
                , letBoundBindage      :: Bindage
                , letBoundType         :: Type
                }

data FParamEntry lore =
  FParamEntry { fparamRange        :: ScalExpRange
              , paramLore         :: Lore.FParam lore
              , fparamBindingDepth :: Int
              , paramType         :: Type
              }

data LParamEntry lore =
  LParamEntry { lparamRange        :: ScalExpRange
              , lparamBindingDepth :: Int
              , lparamType         :: Type
              }

data FreeVarEntry =
  FreeVarEntry { freeVarType         :: Type
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

entryLetBoundLore :: Entry lore -> Maybe (Lore.LetBound lore)
entryLetBoundLore (LetBound entry) = Just $ letBoundLore entry
entryLetBoundLore _                = Nothing

entryFParamLore :: Entry lore -> Maybe (Lore.FParam lore)
entryFParamLore (FParam entry) = Just $ paramLore entry
entryFParamLore _              = Nothing

loopVariable :: Entry lore -> Bool
loopVariable (LoopVar _) = True
loopVariable _           = False

asExp :: Entry lore -> Maybe (Exp lore)
asExp = liftM (bindingExp . letBoundBinding) . isVarBound

entryType :: Entry lore -> Type
entryType (LetBound entry) = letBoundType entry
entryType (LParam entry)   = lparamType entry
entryType (FParam entry)   = paramType entry
entryType (LoopVar _)      = Basic Int
entryType (FreeVar entry)  = freeVarType entry

instance Substitutable lore => Substitute (LetBoundEntry lore) where
  substituteNames substs entry =
    LetBoundEntry {
        letBoundRange = substituteNames substs $ letBoundRange entry
      , letBoundLore = substituteNames substs $ letBoundLore entry
      , letBoundBinding = substituteNames substs $ letBoundBinding entry
      , letBoundScalExp = substituteNames substs $ letBoundScalExp entry
      , letBoundBindingDepth = letBoundBindingDepth entry
      , letBoundBindage = substituteNames substs $ letBoundBindage entry
      , letBoundType = substituteNames substs $ letBoundType entry
      }

instance Substitutable lore => Substitute (FParamEntry lore) where
  substituteNames substs entry =
    FParamEntry {
          fparamRange = substituteNames substs $ fparamRange entry
        , paramLore = substituteNames substs $ paramLore entry
        , fparamBindingDepth = fparamBindingDepth entry
        , paramType = substituteNames substs $ paramType entry
      }

instance Substitutable lore => Substitute (LParamEntry lore) where
  substituteNames substs entry =
    LParamEntry {
          lparamRange = substituteNames substs $ lparamRange entry
        , lparamBindingDepth = lparamBindingDepth entry
        , lparamType = substituteNames substs $ lparamType entry
      }

instance Substitutable lore => Substitute (LoopVarEntry lore) where
  substituteNames substs entry =
    LoopVarEntry {
          loopVarRange = substituteNames substs $ loopVarRange entry
        , loopVarBindingDepth = loopVarBindingDepth entry
      }

instance Substitute FreeVarEntry where
  substituteNames substs entry =
    FreeVarEntry {
        freeVarRange = substituteNames substs $ freeVarRange entry
      , freeVarType = substituteNames substs $ freeVarType entry
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

lookupType :: VName -> SymbolTable lore -> Maybe Type
lookupType name vtable = entryType <$> lookup name vtable

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

typeEnv :: SymbolTable lore -> TypeEnv
typeEnv = HM.map entryType . bindings

defBndEntry :: SymbolTable lore
            -> PatElem lore
            -> Range
            -> Binding lore
            -> LetBoundEntry lore
defBndEntry vtable patElem range bnd =
  LetBoundEntry {
      letBoundRange = simplifyRange $ scalExpRange range
    , letBoundLore = patElemLore patElem
    , letBoundBinding = bnd
    , letBoundScalExp =
      runReader (toScalExp (`lookupScalExp` vtable) (bindingExp bnd)) types
    , letBoundBindingDepth = 0
    , letBoundBindage = patElemBindage patElem
    , letBoundType = patElemType patElem
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

        simplifyBound (Just se)
          | Right se' <- AS.simplify se ranges =
            Just se'
        simplifyBound bound =
          bound

bindingEntries :: Ranged lore =>
                  Binding lore -> SymbolTable lore
               -> [LetBoundEntry lore]
bindingEntries bnd@(Let pat _ _) vtable =
  [ defBndEntry vtable patElem range bnd |
    (patElem, range) <- zip (patternElements pat) (Ranges.patternRanges pat)
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

insertFParam :: AST.FParam lore
             -> SymbolTable lore
             -> SymbolTable lore
insertFParam fparam = insertEntry name entry
  where name = paramName fparam
        entry = FParam FParamEntry { fparamRange = (Nothing, Nothing)
                                   , paramLore = AST.paramLore fparam
                                   , fparamBindingDepth = 0
                                   , paramType = AST.paramType fparam
                                   }

insertFParams :: [AST.FParam lore] -> SymbolTable lore
              -> SymbolTable lore
insertFParams fparams symtable = foldr insertFParam symtable fparams

insertLParamWithRange :: LParam lore -> ScalExpRange -> SymbolTable lore
                      -> SymbolTable lore
insertLParamWithRange param range vtable =
  -- We know that the sizes in the type of param are at least zero,
  -- since they are array sizes.
  let vtable' = insertEntry name bind vtable
  in foldr (`isAtLeast` 0) vtable' sizevars
  where bind = LParam LParamEntry { lparamRange = range
                                  , lparamBindingDepth = 0
                                  , lparamType = AST.paramType param
                                  }
        name = paramName param
        sizevars = mapMaybe isVar $ arrayDims $ AST.paramType param
        isVar (Var v) = Just v
        isVar _       = Nothing

insertLParam :: LParam lore -> SymbolTable lore -> SymbolTable lore
insertLParam param =
  insertLParamWithRange param (Nothing, Nothing)

insertArrayLParam :: LParam lore -> Maybe VName -> SymbolTable lore
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
            loopVarRange = (Just (Val (IntVal 0)),
                            Just $
                              subExpToScalExp bound Int `SMinus`
                              Val (IntVal 1))
          , loopVarBindingDepth = 0
          }

insertFreeVar :: VName -> Type -> SymbolTable lore -> SymbolTable lore
insertFreeVar name t = insertEntry name entry
  where entry = FreeVar FreeVarEntry {
            freeVarType = t
          , freeVarRange = (Nothing, Nothing)
          , freeVarBindingDepth = 0
          }

updateBounds :: Bool -> SubExp -> SymbolTable lore -> SymbolTable lore
updateBounds isTrue cond vtable =
  case runReader (toScalExp (`lookupScalExp` vtable) $ PrimOp $ SubExp cond) types of
    Nothing    -> vtable
    Just cond' ->
      let cond'' | isTrue    = cond'
                 | otherwise = SNot cond'
      in updateBounds' cond'' vtable
  where types = typeEnv vtable

-- | Refines the ranges in the symbol table with
--     ranges extracted from branch conditions.
--   `cond' is the condition of the if-branch.
updateBounds' :: ScalExp -> SymbolTable lore -> SymbolTable lore
updateBounds' cond sym_tab =
  foldr updateBound sym_tab $ mapMaybe solve_leq0 $
  either (const []) getNotFactorsLEQ0 $
  AS.simplify (SNot cond) ranges
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
      solve_leq0 :: ScalExp -> Maybe (VName, Bool, ScalExp)
      solve_leq0 e_scal = do
        sym <- AS.pickSymToElim ranges S.empty e_scal
        (a,b) <- either (const Nothing) id $ AS.linFormScalE sym e_scal ranges
        case a of
          Val (IntVal (-1)) -> Just (sym, False, b)
          Val (IntVal 1)    -> do
            mb <- either (const Nothing) Just $
                  AS.simplify (SMinus (Val (IntVal 0)) b) ranges
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
