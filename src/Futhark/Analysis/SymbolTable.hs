{-# LANGUAGE TypeFamilies #-}

module Futhark.Analysis.SymbolTable
  ( SymbolTable (bindings, loopDepth, availableAtClosestLoop, simplifyMemory),
    empty,
    fromScope,
    toScope,

    -- * Entries
    Entry,
    deepen,
    entryAccInput,
    entryDepth,
    entryLetBoundDec,
    entryIsSize,
    entryStm,
    entryFParam,
    entryLParam,

    -- * Lookup
    elem,
    lookup,
    lookupStm,
    lookupExp,
    lookupBasicOp,
    lookupType,
    lookupSubExp,
    lookupAliases,
    lookupLoopVar,
    lookupLoopParam,
    aliases,
    available,
    subExpAvailable,
    consume,
    index,
    index',
    Indexed (..),
    indexedAddCerts,
    IndexOp (..),

    -- * Insertion
    insertStm,
    insertStms,
    insertFParams,
    insertLParam,
    insertLoopVar,
    insertLoopMerge,

    -- * Misc
    hideCertified,
    noteAccTokens,
  )
where

import Control.Arrow ((&&&))
import Control.Monad
import Data.List (elemIndex, foldl')
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ord
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR hiding (FParam, lookupType)
import Futhark.IR qualified as AST
import Futhark.IR.Prop.Aliases qualified as Aliases
import Prelude hiding (elem, lookup)

data SymbolTable rep = SymbolTable
  { loopDepth :: Int,
    bindings :: M.Map VName (Entry rep),
    -- | Which names are available just before the most enclosing
    -- loop?
    availableAtClosestLoop :: Names,
    -- | We are in a situation where we should
    -- simplify/hoist/un-existentialise memory as much as possible -
    -- typically, inside a kernel.
    simplifyMemory :: Bool
  }

instance Semigroup (SymbolTable rep) where
  table1 <> table2 =
    SymbolTable
      { loopDepth = max (loopDepth table1) (loopDepth table2),
        bindings = bindings table1 <> bindings table2,
        availableAtClosestLoop =
          availableAtClosestLoop table1
            <> availableAtClosestLoop table2,
        simplifyMemory = simplifyMemory table1 || simplifyMemory table2
      }

instance Monoid (SymbolTable rep) where
  mempty = empty

empty :: SymbolTable rep
empty = SymbolTable 0 M.empty mempty False

fromScope :: (ASTRep rep) => Scope rep -> SymbolTable rep
fromScope = M.foldlWithKey' insertFreeVar' empty
  where
    insertFreeVar' m k dec = insertFreeVar k dec m

toScope :: SymbolTable rep -> Scope rep
toScope = M.map entryInfo . bindings

deepen :: SymbolTable rep -> SymbolTable rep
deepen vtable =
  vtable
    { loopDepth = loopDepth vtable + 1,
      availableAtClosestLoop = namesFromList $ M.keys $ bindings vtable
    }

-- | The result of indexing a delayed array.
data Indexed
  = -- | A PrimExp based on the indexes (that is, without
    -- accessing any actual array).
    Indexed Certs (PrimExp VName)
  | -- | The indexing corresponds to another (perhaps more
    -- advantageous) array.
    IndexedArray Certs VName [TPrimExp Int64 VName]

indexedAddCerts :: Certs -> Indexed -> Indexed
indexedAddCerts cs1 (Indexed cs2 v) = Indexed (cs1 <> cs2) v
indexedAddCerts cs1 (IndexedArray cs2 arr v) = IndexedArray (cs1 <> cs2) arr v

instance FreeIn Indexed where
  freeIn' (Indexed cs v) = freeIn' cs <> freeIn' v
  freeIn' (IndexedArray cs arr v) = freeIn' cs <> freeIn' arr <> freeIn' v

-- | Indexing a delayed array if possible.
type IndexArray = [TPrimExp Int64 VName] -> Maybe Indexed

data Entry rep = Entry
  { -- | True if consumed.
    entryConsumed :: Bool,
    entryDepth :: Int,
    -- | True if this name has been used as an array size,
    -- implying that it is non-negative.
    entryIsSize :: Bool,
    -- | For names that are tokens of an accumulator, this is the
    -- corresponding combining function and neutral element.
    entryAccInput :: Maybe (WithAccInput rep),
    entryType :: EntryType rep
  }

data EntryType rep
  = LoopVar (LoopVarEntry rep)
  | LetBound (LetBoundEntry rep)
  | FParam (FParamEntry rep)
  | LParam (LParamEntry rep)
  | FreeVar (FreeVarEntry rep)

data LoopVarEntry rep = LoopVarEntry
  { loopVarType :: IntType,
    loopVarBound :: SubExp
  }

data LetBoundEntry rep = LetBoundEntry
  { letBoundDec :: LetDec rep,
    letBoundAliases :: Names,
    letBoundStm :: Stm rep,
    -- | Index a delayed array, if possible.
    letBoundIndex :: Int -> IndexArray
  }

data FParamEntry rep = FParamEntry
  { fparamDec :: FParamInfo rep,
    fparamAliases :: Names,
    -- | If a loop parameter, the initial value and the eventual
    -- result.  The result need not be in scope in the symbol table.
    fparamMerge :: Maybe (SubExp, SubExp)
  }

data LParamEntry rep = LParamEntry
  { lparamDec :: LParamInfo rep,
    lparamAliases :: Names,
    lparamIndex :: IndexArray
  }

data FreeVarEntry rep = FreeVarEntry
  { freeVarDec :: NameInfo rep,
    freeVarAliases :: Names,
    -- | Index a delayed array, if possible.
    freeVarIndex :: VName -> IndexArray
  }

instance (ASTRep rep) => Typed (Entry rep) where
  typeOf = typeOf . entryInfo

entryInfo :: Entry rep -> NameInfo rep
entryInfo e = case entryType e of
  LetBound entry -> LetName $ letBoundDec entry
  LoopVar entry -> IndexName $ loopVarType entry
  FParam entry -> FParamName $ fparamDec entry
  LParam entry -> LParamName $ lparamDec entry
  FreeVar entry -> freeVarDec entry

isLetBound :: Entry rep -> Maybe (LetBoundEntry rep)
isLetBound e = case entryType e of
  LetBound entry -> Just entry
  _ -> Nothing

entryStm :: Entry rep -> Maybe (Stm rep)
entryStm = fmap letBoundStm . isLetBound

entryFParam :: Entry rep -> Maybe (FParamInfo rep)
entryFParam e = case entryType e of
  FParam e' -> Just $ fparamDec e'
  _ -> Nothing

entryLParam :: Entry rep -> Maybe (LParamInfo rep)
entryLParam e = case entryType e of
  LParam e' -> Just $ lparamDec e'
  _ -> Nothing

entryLetBoundDec :: Entry rep -> Maybe (LetDec rep)
entryLetBoundDec = fmap letBoundDec . isLetBound

entryAliases :: EntryType rep -> Names
entryAliases (LetBound e) = letBoundAliases e
entryAliases (FParam e) = fparamAliases e
entryAliases (LParam e) = lparamAliases e
entryAliases (FreeVar e) = freeVarAliases e
entryAliases (LoopVar _) = mempty -- Integers have no aliases.

-- | You almost always want 'available' instead of this one.
elem :: VName -> SymbolTable rep -> Bool
elem name = isJust . lookup name

lookup :: VName -> SymbolTable rep -> Maybe (Entry rep)
lookup name = M.lookup name . bindings

lookupStm :: VName -> SymbolTable rep -> Maybe (Stm rep)
lookupStm name vtable = entryStm =<< lookup name vtable

lookupExp :: VName -> SymbolTable rep -> Maybe (Exp rep, Certs)
lookupExp name vtable = (stmExp &&& stmCerts) <$> lookupStm name vtable

lookupBasicOp :: VName -> SymbolTable rep -> Maybe (BasicOp, Certs)
lookupBasicOp name vtable = case lookupExp name vtable of
  Just (BasicOp e, cs) -> Just (e, cs)
  _ -> Nothing

lookupType :: (ASTRep rep) => VName -> SymbolTable rep -> Maybe Type
lookupType name vtable = typeOf <$> lookup name vtable

lookupSubExpType :: (ASTRep rep) => SubExp -> SymbolTable rep -> Maybe Type
lookupSubExpType (Var v) = lookupType v
lookupSubExpType (Constant v) = const $ Just $ Prim $ primValueType v

lookupSubExp :: VName -> SymbolTable rep -> Maybe (SubExp, Certs)
lookupSubExp name vtable = do
  (e, cs) <- lookupExp name vtable
  case e of
    BasicOp (SubExp se) -> Just (se, cs)
    _ -> Nothing

lookupAliases :: VName -> SymbolTable rep -> Names
lookupAliases name vtable =
  maybe mempty (entryAliases . entryType) $ M.lookup name (bindings vtable)

-- | If the given variable name is the name of a 'ForLoop' parameter,
-- then return the bound of that loop.
lookupLoopVar :: VName -> SymbolTable rep -> Maybe SubExp
lookupLoopVar name vtable = do
  LoopVar e <- entryType <$> M.lookup name (bindings vtable)
  pure $ loopVarBound e

-- | Look up the initial value and eventual result of a loop
-- parameter.  Note that the result almost certainly refers to
-- something that is not part of the symbol table.
lookupLoopParam :: VName -> SymbolTable rep -> Maybe (SubExp, SubExp)
lookupLoopParam name vtable = do
  FParam e <- entryType <$> M.lookup name (bindings vtable)
  fparamMerge e

-- | Do these two names alias each other?  This is expected to be a
-- commutative relationship, so the order of arguments does not
-- matter.
aliases :: VName -> VName -> SymbolTable rep -> Bool
aliases x y vtable = x == y || (x `nameIn` lookupAliases y vtable)

-- | In symbol table and not consumed.
available :: VName -> SymbolTable rep -> Bool
available name = maybe False (not . entryConsumed) . M.lookup name . bindings

-- | Constant or 'available'
subExpAvailable :: SubExp -> SymbolTable rep -> Bool
subExpAvailable (Var name) = available name
subExpAvailable Constant {} = const True

index' ::
  VName ->
  [TPrimExp Int64 VName] ->
  SymbolTable rep ->
  Maybe Indexed
index' name is vtable = do
  entry <- lookup name vtable
  case entryType entry of
    LetBound entry'
      | Just k <-
          elemIndex name . patNames . stmPat $ letBoundStm entry' ->
          letBoundIndex entry' k is
    FreeVar entry' ->
      freeVarIndex entry' name is
    LParam entry' -> lparamIndex entry' is
    _ -> Nothing

-- | @index arr is vtable@ fully indexes the array @arr@ at position @is@ using
-- information in @vtable@, and produces the symbolic result of the indexing if
-- it can be expressed. This is essentially a form of pull-array indexing.
index ::
  (ASTRep rep) =>
  VName ->
  [SubExp] ->
  SymbolTable rep ->
  Maybe Indexed
index name is table = do
  is' <- mapM asPrimExp is
  index' name is' table
  where
    asPrimExp i = do
      Prim t <- lookupSubExpType i table
      pure $ TPrimExp $ primExpFromSubExp t i

class IndexOp op where
  indexOp ::
    (ASTRep rep, IndexOp (Op rep)) =>
    SymbolTable rep ->
    Int ->
    op ->
    [TPrimExp Int64 VName] ->
    Maybe Indexed
  indexOp _ _ _ _ = Nothing

instance IndexOp (NoOp rep)

indexExp ::
  (IndexOp (Op rep), ASTRep rep) =>
  SymbolTable rep ->
  Exp rep ->
  -- | Index of result being indexed in case the expression produces more than
  -- one.
  Int ->
  IndexArray
indexExp vtable (Op op) k is =
  indexOp vtable k op is
indexExp _ (BasicOp (Iota _ x s to_it)) _ [i] =
  Just $
    Indexed mempty $
      ( sExt to_it (untyped i)
          `mul` primExpFromSubExp (IntType to_it) s
      )
        `add` primExpFromSubExp (IntType to_it) x
  where
    mul = BinOpExp (Mul to_it OverflowWrap)
    add = BinOpExp (Add to_it OverflowWrap)
indexExp table (BasicOp (Replicate (Shape ds) v)) _ is
  | length ds == length is,
    Just (Prim t) <- lookupSubExpType v table =
      Just $ Indexed mempty $ primExpFromSubExp t v
indexExp table (BasicOp (Replicate s (Var v))) _ is = do
  guard $ v `available` table
  guard $ s /= mempty
  index' v (drop (shapeRank s) is) table
indexExp table (BasicOp (Reshape v newshape)) _ is
  | Just oldshape <- arrayDims <$> lookupType v table =
      -- TODO: handle coercions more efficiently.
      let is' =
            reshapeIndex
              (map pe64 oldshape)
              (map pe64 $ shapeDims $ newShape newshape)
              is
       in index' v is' table
indexExp table (BasicOp (Index v slice)) _ is = do
  guard $ v `available` table
  index' v (adjust (unSlice slice) is) table
  where
    adjust (DimFix j : js') is' =
      pe64 j : adjust js' is'
    adjust (DimSlice j _ s : js') (i : is') =
      let i_t_s = i * pe64 s
          j_p_i_t_s = pe64 j + i_t_s
       in j_p_i_t_s : adjust js' is'
    adjust _ _ = []
indexExp _ _ _ _ = Nothing

defBndEntry ::
  (ASTRep rep, IndexOp (Op rep)) =>
  SymbolTable rep ->
  PatElem (LetDec rep) ->
  Names ->
  Stm rep ->
  LetBoundEntry rep
defBndEntry vtable patElem als stm =
  LetBoundEntry
    { letBoundDec = patElemDec patElem,
      letBoundAliases = als,
      letBoundStm = stm,
      letBoundIndex = \k ->
        fmap (indexedAddCerts (stmAuxCerts $ stmAux stm))
          . indexExp vtable (stmExp stm) k
    }

bindingEntries ::
  (Aliases.Aliased rep, IndexOp (Op rep)) =>
  Stm rep ->
  SymbolTable rep ->
  [LetBoundEntry rep]
bindingEntries stm@(Let pat _ _) vtable = do
  pat_elem <- patElems pat
  pure $ defBndEntry vtable pat_elem (expandAliases (Aliases.aliasesOf pat_elem) vtable) stm

adjustSeveral :: (Ord k) => (v -> v) -> [k] -> M.Map k v -> M.Map k v
adjustSeveral f = flip $ foldl' $ flip $ M.adjust f

insertEntry ::
  (ASTRep rep) =>
  VName ->
  EntryType rep ->
  SymbolTable rep ->
  SymbolTable rep
insertEntry name entry vtable =
  let entry' =
        Entry
          { entryConsumed = False,
            entryDepth = loopDepth vtable,
            entryIsSize = False,
            entryAccInput = Nothing,
            entryType = entry
          }
      dims = mapMaybe subExpVar $ arrayDims $ typeOf entry'
      isSize e = e {entryIsSize = True}
   in vtable
        { bindings =
            adjustSeveral isSize dims $
              M.insert name entry' $
                bindings vtable
        }

insertEntries ::
  (ASTRep rep) =>
  [(VName, EntryType rep)] ->
  SymbolTable rep ->
  SymbolTable rep
insertEntries entries vtable =
  foldl' add vtable entries
  where
    add vtable' (name, entry) = insertEntry name entry vtable'

insertStm ::
  (IndexOp (Op rep), Aliases.Aliased rep) =>
  Stm rep ->
  SymbolTable rep ->
  SymbolTable rep
insertStm stm vtable =
  flip (foldl' $ flip consume) (namesToList stm_consumed) $
    flip (foldl' addRevAliases) (zip names entries) $
      insertEntries (zip names $ map LetBound entries) vtable
  where
    entries = bindingEntries stm vtable
    names = patNames $ stmPat stm
    stm_consumed = expandAliases (Aliases.consumedInStm stm) vtable
    addRevAliases vtable' (name, LetBoundEntry {letBoundAliases = als}) =
      vtable' {bindings = adjustSeveral update inedges $ bindings vtable'}
      where
        inedges = namesToList $ expandAliases als vtable'
        update e = e {entryType = update' $ entryType e}
        update' (LetBound entry) =
          LetBound
            entry
              { letBoundAliases = oneName name <> letBoundAliases entry
              }
        update' (FParam entry) =
          FParam
            entry
              { fparamAliases = oneName name <> fparamAliases entry
              }
        update' (LParam entry) =
          LParam
            entry
              { lparamAliases = oneName name <> lparamAliases entry
              }
        update' (FreeVar entry) =
          FreeVar
            entry
              { freeVarAliases = oneName name <> freeVarAliases entry
              }
        update' e = e

insertStms ::
  (IndexOp (Op rep), Aliases.Aliased rep) =>
  Stms rep ->
  SymbolTable rep ->
  SymbolTable rep
insertStms stms vtable = foldl' (flip insertStm) vtable $ stmsToList stms

expandAliases :: Names -> SymbolTable rep -> Names
expandAliases names vtable = names <> aliasesOfAliases
  where
    aliasesOfAliases =
      mconcat . map (`lookupAliases` vtable) . namesToList $ names

insertFParam ::
  (ASTRep rep) =>
  AST.FParam rep ->
  SymbolTable rep ->
  SymbolTable rep
insertFParam fparam = insertEntry name entry
  where
    name = AST.paramName fparam
    entry =
      FParam
        FParamEntry
          { fparamDec = AST.paramDec fparam,
            fparamAliases = mempty,
            fparamMerge = Nothing
          }

insertFParams ::
  (ASTRep rep) =>
  [AST.FParam rep] ->
  SymbolTable rep ->
  SymbolTable rep
insertFParams fparams symtable = foldl' (flip insertFParam) symtable fparams

insertLParam :: (ASTRep rep) => LParam rep -> SymbolTable rep -> SymbolTable rep
insertLParam param = insertEntry name bind
  where
    bind =
      LParam
        LParamEntry
          { lparamDec = AST.paramDec param,
            lparamAliases = mempty,
            lparamIndex = const Nothing
          }
    name = AST.paramName param

-- | Insert entries corresponding to the parameters of a loop (not
-- distinguishing contect and value part).  Apart from the parameter
-- itself, we also insert the initial value and the subexpression
-- providing the final value.  Note that the latter is likely not in
-- scope in the symbol at this point.  This is OK, and can still be
-- used to help some loop optimisations detect invariant loop
-- parameters.
insertLoopMerge ::
  (ASTRep rep) =>
  [(AST.FParam rep, SubExp, SubExpRes)] ->
  SymbolTable rep ->
  SymbolTable rep
insertLoopMerge = flip $ foldl' $ flip bind
  where
    bind (p, initial, SubExpRes _ res) =
      insertEntry (paramName p) $
        FParam
          FParamEntry
            { fparamDec = AST.paramDec p,
              fparamAliases = mempty,
              fparamMerge = Just (initial, res)
            }

insertLoopVar :: (ASTRep rep) => VName -> IntType -> SubExp -> SymbolTable rep -> SymbolTable rep
insertLoopVar name it bound = insertEntry name bind
  where
    bind =
      LoopVar
        LoopVarEntry
          { loopVarType = it,
            loopVarBound = bound
          }

insertFreeVar :: (ASTRep rep) => VName -> NameInfo rep -> SymbolTable rep -> SymbolTable rep
insertFreeVar name dec = insertEntry name entry
  where
    entry =
      FreeVar
        FreeVarEntry
          { freeVarDec = dec,
            freeVarIndex = \_ _ -> Nothing,
            freeVarAliases = mempty
          }

consume :: VName -> SymbolTable rep -> SymbolTable rep
consume consumee vtable =
  foldl' consume' vtable $
    namesToList $
      expandAliases (oneName consumee) vtable
  where
    consume' vtable' v =
      vtable' {bindings = M.adjust consume'' v $ bindings vtable'}
    consume'' e = e {entryConsumed = True}

-- | Hide definitions of those entries that satisfy some predicate.
hideIf :: (Entry rep -> Bool) -> SymbolTable rep -> SymbolTable rep
hideIf hide vtable = vtable {bindings = M.map maybeHide $ bindings vtable}
  where
    maybeHide entry
      | hide entry =
          entry
            { entryType =
                FreeVar
                  FreeVarEntry
                    { freeVarDec = entryInfo entry,
                      freeVarIndex = \_ _ -> Nothing,
                      freeVarAliases = entryAliases $ entryType entry
                    }
            }
      | otherwise = entry

-- | Hide these definitions, if they are protected by certificates in
-- the set of names.
hideCertified :: Names -> SymbolTable rep -> SymbolTable rep
hideCertified to_hide = hideIf $ maybe False hide . entryStm
  where
    hide = any (`nameIn` to_hide) . unCerts . stmCerts

-- | Note that these names are tokens for the corresponding
-- accumulators.  The names must already be present in the symbol
-- table.
noteAccTokens ::
  [(VName, WithAccInput rep)] ->
  SymbolTable rep ->
  SymbolTable rep
noteAccTokens = flip (foldl' f)
  where
    f vtable (v, accum) =
      case M.lookup v $ bindings vtable of
        Nothing -> vtable
        Just e ->
          vtable
            { bindings =
                M.insert v (e {entryAccInput = Just accum}) $ bindings vtable
            }
