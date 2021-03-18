{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Analysis.SymbolTable
  ( SymbolTable (bindings, loopDepth, availableAtClosestLoop, simplifyMemory),
    empty,
    fromScope,
    toScope,

    -- * Entries
    Entry,
    deepen,
    entryDepth,
    entryLetBoundDec,
    entryIsSize,

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
    available,
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
  )
where

import Control.Arrow ((&&&))
import Control.Monad
import Data.List (elemIndex, foldl')
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR hiding (FParam, lookupType)
import qualified Futhark.IR as AST
import qualified Futhark.IR.Prop.Aliases as Aliases
import Prelude hiding (elem, lookup)

data SymbolTable lore = SymbolTable
  { loopDepth :: Int,
    bindings :: M.Map VName (Entry lore),
    -- | Which names are available just before the most enclosing
    -- loop?
    availableAtClosestLoop :: Names,
    -- | We are in a situation where we should
    -- simplify/hoist/un-existentialise memory as much as possible -
    -- typically, inside a kernel.
    simplifyMemory :: Bool
  }

instance Semigroup (SymbolTable lore) where
  table1 <> table2 =
    SymbolTable
      { loopDepth = max (loopDepth table1) (loopDepth table2),
        bindings = bindings table1 <> bindings table2,
        availableAtClosestLoop =
          availableAtClosestLoop table1
            <> availableAtClosestLoop table2,
        simplifyMemory = simplifyMemory table1 || simplifyMemory table2
      }

instance Monoid (SymbolTable lore) where
  mempty = empty

empty :: SymbolTable lore
empty = SymbolTable 0 M.empty mempty False

fromScope :: ASTLore lore => Scope lore -> SymbolTable lore
fromScope = M.foldlWithKey' insertFreeVar' empty
  where
    insertFreeVar' m k dec = insertFreeVar k dec m

toScope :: SymbolTable lore -> Scope lore
toScope = M.map entryInfo . bindings

deepen :: SymbolTable lore -> SymbolTable lore
deepen vtable =
  vtable
    { loopDepth = loopDepth vtable + 1,
      availableAtClosestLoop = namesFromList $ M.keys $ bindings vtable
    }

-- | The result of indexing a delayed array.
data Indexed
  = -- | A PrimExp based on the indexes (that is, without
    -- accessing any actual array).
    Indexed Certificates (PrimExp VName)
  | -- | The indexing corresponds to another (perhaps more
    -- advantageous) array.
    IndexedArray Certificates VName [TPrimExp Int64 VName]

indexedAddCerts :: Certificates -> Indexed -> Indexed
indexedAddCerts cs1 (Indexed cs2 v) = Indexed (cs1 <> cs2) v
indexedAddCerts cs1 (IndexedArray cs2 arr v) = IndexedArray (cs1 <> cs2) arr v

instance FreeIn Indexed where
  freeIn' (Indexed cs v) = freeIn' cs <> freeIn' v
  freeIn' (IndexedArray cs arr v) = freeIn' cs <> freeIn' arr <> freeIn' v

-- | Indexing a delayed array if possible.
type IndexArray = [TPrimExp Int64 VName] -> Maybe Indexed

data Entry lore = Entry
  { -- | True if consumed.
    entryConsumed :: Bool,
    entryDepth :: Int,
    -- | True if this name has been used as an array size,
    -- implying that it is non-negative.
    entryIsSize :: Bool,
    entryType :: EntryType lore
  }

data EntryType lore
  = LoopVar (LoopVarEntry lore)
  | LetBound (LetBoundEntry lore)
  | FParam (FParamEntry lore)
  | LParam (LParamEntry lore)
  | FreeVar (FreeVarEntry lore)

data LoopVarEntry lore = LoopVarEntry
  { loopVarType :: IntType,
    loopVarBound :: SubExp
  }

data LetBoundEntry lore = LetBoundEntry
  { letBoundDec :: LetDec lore,
    letBoundAliases :: Names,
    letBoundStm :: Stm lore,
    -- | Index a delayed array, if possible.
    letBoundIndex :: Int -> IndexArray
  }

data FParamEntry lore = FParamEntry
  { fparamDec :: FParamInfo lore,
    fparamAliases :: Names,
    -- | If a loop parameter, the initial value and the eventual
    -- result.  The result need not be in scope in the symbol table.
    fparamMerge :: Maybe (SubExp, SubExp)
  }

data LParamEntry lore = LParamEntry
  { lparamDec :: LParamInfo lore,
    lparamIndex :: IndexArray
  }

data FreeVarEntry lore = FreeVarEntry
  { freeVarDec :: NameInfo lore,
    -- | Index a delayed array, if possible.
    freeVarIndex :: VName -> IndexArray
  }

instance ASTLore lore => Typed (Entry lore) where
  typeOf = typeOf . entryInfo

entryInfo :: Entry lore -> NameInfo lore
entryInfo e = case entryType e of
  LetBound entry -> LetName $ letBoundDec entry
  LoopVar entry -> IndexName $ loopVarType entry
  FParam entry -> FParamName $ fparamDec entry
  LParam entry -> LParamName $ lparamDec entry
  FreeVar entry -> freeVarDec entry

isLetBound :: Entry lore -> Maybe (LetBoundEntry lore)
isLetBound e = case entryType e of
  LetBound entry -> Just entry
  _ -> Nothing

entryStm :: Entry lore -> Maybe (Stm lore)
entryStm = fmap letBoundStm . isLetBound

entryLetBoundDec :: Entry lore -> Maybe (LetDec lore)
entryLetBoundDec = fmap letBoundDec . isLetBound

elem :: VName -> SymbolTable lore -> Bool
elem name = isJust . lookup name

lookup :: VName -> SymbolTable lore -> Maybe (Entry lore)
lookup name = M.lookup name . bindings

lookupStm :: VName -> SymbolTable lore -> Maybe (Stm lore)
lookupStm name vtable = entryStm =<< lookup name vtable

lookupExp :: VName -> SymbolTable lore -> Maybe (Exp lore, Certificates)
lookupExp name vtable = (stmExp &&& stmCerts) <$> lookupStm name vtable

lookupBasicOp :: VName -> SymbolTable lore -> Maybe (BasicOp, Certificates)
lookupBasicOp name vtable = case lookupExp name vtable of
  Just (BasicOp e, cs) -> Just (e, cs)
  _ -> Nothing

lookupType :: ASTLore lore => VName -> SymbolTable lore -> Maybe Type
lookupType name vtable = typeOf <$> lookup name vtable

lookupSubExpType :: ASTLore lore => SubExp -> SymbolTable lore -> Maybe Type
lookupSubExpType (Var v) = lookupType v
lookupSubExpType (Constant v) = const $ Just $ Prim $ primValueType v

lookupSubExp :: VName -> SymbolTable lore -> Maybe (SubExp, Certificates)
lookupSubExp name vtable = do
  (e, cs) <- lookupExp name vtable
  case e of
    BasicOp (SubExp se) -> Just (se, cs)
    _ -> Nothing

lookupAliases :: VName -> SymbolTable lore -> Names
lookupAliases name vtable =
  case entryType <$> M.lookup name (bindings vtable) of
    Just (LetBound e) -> letBoundAliases e
    Just (FParam e) -> fparamAliases e
    _ -> mempty

-- | If the given variable name is the name of a 'ForLoop' parameter,
-- then return the bound of that loop.
lookupLoopVar :: VName -> SymbolTable lore -> Maybe SubExp
lookupLoopVar name vtable = do
  LoopVar e <- entryType <$> M.lookup name (bindings vtable)
  return $ loopVarBound e

lookupLoopParam :: VName -> SymbolTable lore -> Maybe (SubExp, SubExp)
lookupLoopParam name vtable = do
  FParam e <- entryType <$> M.lookup name (bindings vtable)
  fparamMerge e

-- | In symbol table and not consumed.
available :: VName -> SymbolTable lore -> Bool
available name = maybe False (not . entryConsumed) . M.lookup name . bindings

index ::
  ASTLore lore =>
  VName ->
  [SubExp] ->
  SymbolTable lore ->
  Maybe Indexed
index name is table = do
  is' <- mapM asPrimExp is
  index' name is' table
  where
    asPrimExp i = do
      Prim t <- lookupSubExpType i table
      return $ TPrimExp $ primExpFromSubExp t i

index' ::
  VName ->
  [TPrimExp Int64 VName] ->
  SymbolTable lore ->
  Maybe Indexed
index' name is vtable = do
  entry <- lookup name vtable
  case entryType entry of
    LetBound entry'
      | Just k <-
          elemIndex name $
            patternValueNames $
              stmPattern $ letBoundStm entry' ->
        letBoundIndex entry' k is
    FreeVar entry' ->
      freeVarIndex entry' name is
    LParam entry' -> lparamIndex entry' is
    _ -> Nothing

class IndexOp op where
  indexOp ::
    (ASTLore lore, IndexOp (Op lore)) =>
    SymbolTable lore ->
    Int ->
    op ->
    [TPrimExp Int64 VName] ->
    Maybe Indexed
  indexOp _ _ _ _ = Nothing

instance IndexOp ()

indexExp ::
  (IndexOp (Op lore), ASTLore lore) =>
  SymbolTable lore ->
  Exp lore ->
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
indexExp table (BasicOp (Replicate (Shape [_]) (Var v))) _ (_ : is) =
  index' v is table
indexExp table (BasicOp (Reshape newshape v)) _ is
  | Just oldshape <- arrayDims <$> lookupType v table =
    let is' =
          reshapeIndex
            (map pe64 oldshape)
            (map pe64 $ newDims newshape)
            is
     in index' v is' table
indexExp table (BasicOp (Index v slice)) _ is =
  index' v (adjust slice is) table
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
  (ASTLore lore, IndexOp (Op lore)) =>
  SymbolTable lore ->
  PatElem lore ->
  Names ->
  Stm lore ->
  LetBoundEntry lore
defBndEntry vtable patElem als bnd =
  LetBoundEntry
    { letBoundDec = patElemDec patElem,
      letBoundAliases = als,
      letBoundStm = bnd,
      letBoundIndex = \k ->
        fmap (indexedAddCerts (stmAuxCerts $ stmAux bnd))
          . indexExp vtable (stmExp bnd) k
    }

bindingEntries ::
  (ASTLore lore, Aliases.Aliased lore, IndexOp (Op lore)) =>
  Stm lore ->
  SymbolTable lore ->
  [LetBoundEntry lore]
bindingEntries bnd@(Let pat _ _) vtable = do
  pat_elem <- patternElements pat
  return $ defBndEntry vtable pat_elem (Aliases.aliasesOf pat_elem) bnd

adjustSeveral :: Ord k => (v -> v) -> [k] -> M.Map k v -> M.Map k v
adjustSeveral f = flip $ foldl' $ flip $ M.adjust f

insertEntry ::
  ASTLore lore =>
  VName ->
  EntryType lore ->
  SymbolTable lore ->
  SymbolTable lore
insertEntry name entry vtable =
  let entry' =
        Entry
          { entryConsumed = False,
            entryDepth = loopDepth vtable,
            entryIsSize = False,
            entryType = entry
          }
      dims = mapMaybe subExpVar $ arrayDims $ typeOf entry'
      isSize e = e {entryIsSize = True}
   in vtable
        { bindings =
            adjustSeveral isSize dims $
              M.insert name entry' $ bindings vtable
        }

insertEntries ::
  ASTLore lore =>
  [(VName, EntryType lore)] ->
  SymbolTable lore ->
  SymbolTable lore
insertEntries entries vtable =
  foldl' add vtable entries
  where
    add vtable' (name, entry) = insertEntry name entry vtable'

insertStm ::
  (ASTLore lore, IndexOp (Op lore), Aliases.Aliased lore) =>
  Stm lore ->
  SymbolTable lore ->
  SymbolTable lore
insertStm stm vtable =
  flip (foldl' $ flip consume) (namesToList stm_consumed) $
    flip (foldl' addRevAliases) (patternElements $ stmPattern stm) $
      insertEntries (zip names $ map LetBound $ bindingEntries stm vtable) vtable
  where
    names = patternNames $ stmPattern stm
    stm_consumed = expandAliases (Aliases.consumedInStm stm) vtable
    addRevAliases vtable' pe =
      vtable' {bindings = adjustSeveral update inedges $ bindings vtable'}
      where
        inedges = namesToList $ expandAliases (Aliases.aliasesOf pe) vtable'
        update e = e {entryType = update' $ entryType e}
        update' (LetBound entry) =
          LetBound
            entry
              { letBoundAliases = oneName (patElemName pe) <> letBoundAliases entry
              }
        update' (FParam entry) =
          FParam
            entry
              { fparamAliases = oneName (patElemName pe) <> fparamAliases entry
              }
        update' e = e

insertStms ::
  (ASTLore lore, IndexOp (Op lore), Aliases.Aliased lore) =>
  Stms lore ->
  SymbolTable lore ->
  SymbolTable lore
insertStms stms vtable = foldl' (flip insertStm) vtable $ stmsToList stms

expandAliases :: Names -> SymbolTable lore -> Names
expandAliases names vtable = names <> aliasesOfAliases
  where
    aliasesOfAliases =
      mconcat . map (`lookupAliases` vtable) . namesToList $ names

insertFParam ::
  ASTLore lore =>
  AST.FParam lore ->
  SymbolTable lore ->
  SymbolTable lore
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
  ASTLore lore =>
  [AST.FParam lore] ->
  SymbolTable lore ->
  SymbolTable lore
insertFParams fparams symtable = foldl' (flip insertFParam) symtable fparams

insertLParam :: ASTLore lore => LParam lore -> SymbolTable lore -> SymbolTable lore
insertLParam param = insertEntry name bind
  where
    bind =
      LParam
        LParamEntry
          { lparamDec = AST.paramDec param,
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
  ASTLore lore =>
  [(AST.FParam lore, SubExp, SubExp)] ->
  SymbolTable lore ->
  SymbolTable lore
insertLoopMerge = flip $ foldl' $ flip bind
  where
    bind (p, initial, res) =
      insertEntry (paramName p) $
        FParam
          FParamEntry
            { fparamDec = AST.paramDec p,
              fparamAliases = mempty,
              fparamMerge = Just (initial, res)
            }

insertLoopVar :: ASTLore lore => VName -> IntType -> SubExp -> SymbolTable lore -> SymbolTable lore
insertLoopVar name it bound = insertEntry name bind
  where
    bind =
      LoopVar
        LoopVarEntry
          { loopVarType = it,
            loopVarBound = bound
          }

insertFreeVar :: ASTLore lore => VName -> NameInfo lore -> SymbolTable lore -> SymbolTable lore
insertFreeVar name dec = insertEntry name entry
  where
    entry =
      FreeVar
        FreeVarEntry
          { freeVarDec = dec,
            freeVarIndex = \_ _ -> Nothing
          }

consume :: VName -> SymbolTable lore -> SymbolTable lore
consume consumee vtable =
  foldl' consume' vtable $
    namesToList $
      expandAliases (oneName consumee) vtable
  where
    consume' vtable' v =
      vtable' {bindings = M.adjust consume'' v $ bindings vtable'}
    consume'' e = e {entryConsumed = True}

-- | Hide definitions of those entries that satisfy some predicate.
hideIf :: (Entry lore -> Bool) -> SymbolTable lore -> SymbolTable lore
hideIf hide vtable = vtable {bindings = M.map maybeHide $ bindings vtable}
  where
    maybeHide entry
      | hide entry =
        entry
          { entryType =
              FreeVar
                FreeVarEntry
                  { freeVarDec = entryInfo entry,
                    freeVarIndex = \_ _ -> Nothing
                  }
          }
      | otherwise = entry

-- | Hide these definitions, if they are protected by certificates in
-- the set of names.
hideCertified :: Names -> SymbolTable lore -> SymbolTable lore
hideCertified to_hide = hideIf $ maybe False hide . entryStm
  where
    hide = any (`nameIn` to_hide) . unCertificates . stmCerts
