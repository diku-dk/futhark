{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Analysis.SymbolTable
  ( SymbolTable (bindings, loopDepth, availableAtClosestLoop, simplifyMemory)
  , empty
  , fromScope
  , toScope

    -- * Entries
  , Entry
  , deepen
  , bindingDepth
  , entryStm
  , entryLetBoundDec
  , entryType

    -- * Lookup
  , elem
  , lookup
  , lookupStm
  , lookupExp
  , lookupBasicOp
  , lookupType
  , lookupSubExp
  , lookupAliases
  , lookupLoopVar
  , available
  , consume
  , index
  , index'
  , Indexed(..)
  , indexedAddCerts
  , IndexOp(..)

    -- * Insertion
  , insertStm
  , insertStms
  , insertFParams
  , insertLParam
  , insertLoopVar

    -- * Misc
  , hideIf
  , hideCertified
  )
  where

import Control.Arrow ((&&&))
import Control.Monad
import Data.Ord
import Data.Maybe
import Data.List (foldl', elemIndex)
import qualified Data.Map.Strict as M

import Prelude hiding (elem, lookup)

import Futhark.Analysis.PrimExp.Convert
import Futhark.IR hiding (FParam, lookupType)
import qualified Futhark.IR as AST

import qualified Futhark.IR.Prop.Aliases as Aliases

data SymbolTable lore = SymbolTable {
    loopDepth :: Int
  , bindings :: M.Map VName (Entry lore)
  , availableAtClosestLoop :: Names
    -- ^ Which names are available just before the most enclosing
    -- loop?
  , simplifyMemory :: Bool
    -- ^ We are in a situation where we should
    -- simplify/hoist/un-existentialise memory as much as possible -
    -- typically, inside a kernel.
  }

instance Semigroup (SymbolTable lore) where
  table1 <> table2 =
    SymbolTable { loopDepth = max (loopDepth table1) (loopDepth table2)
                , bindings = bindings table1 <> bindings table2
                , availableAtClosestLoop = availableAtClosestLoop table1 <>
                                           availableAtClosestLoop table2
                , simplifyMemory = simplifyMemory table1 || simplifyMemory table2
                }

instance Monoid (SymbolTable lore) where
  mempty = empty

empty :: SymbolTable lore
empty = SymbolTable 0 M.empty mempty False

fromScope :: Scope lore -> SymbolTable lore
fromScope = M.foldlWithKey' insertFreeVar' empty
  where insertFreeVar' m k dec = insertFreeVar k dec m

toScope :: SymbolTable lore -> Scope lore
toScope = M.map entryInfo . bindings

deepen :: SymbolTable lore -> SymbolTable lore
deepen vtable = vtable { loopDepth = loopDepth vtable + 1,
                         availableAtClosestLoop = namesFromList $ M.keys $ bindings vtable
                       }

-- | The result of indexing a delayed array.
data Indexed = Indexed Certificates (PrimExp VName)
               -- ^ A PrimExp based on the indexes (that is, without
               -- accessing any actual array).
             | IndexedArray Certificates VName [PrimExp VName]
               -- ^ The indexing corresponds to another (perhaps more
               -- advantageous) array.

indexedAddCerts :: Certificates -> Indexed -> Indexed
indexedAddCerts cs1 (Indexed cs2 v) = Indexed (cs1<>cs2) v
indexedAddCerts cs1 (IndexedArray cs2 arr v) = IndexedArray (cs1<>cs2) arr v

instance FreeIn Indexed where
  freeIn' (Indexed cs v) = freeIn' cs <> freeIn' v
  freeIn' (IndexedArray cs arr v) = freeIn' cs <> freeIn' arr <> freeIn' v

-- | Indexing a delayed array if possible.
type IndexArray = [PrimExp VName] -> Maybe Indexed

data Entry lore = LoopVar (LoopVarEntry lore)
                | LetBound (LetBoundEntry lore)
                | FParam (FParamEntry lore)
                | LParam (LParamEntry lore)
                | FreeVar (FreeVarEntry lore)

data LoopVarEntry lore =
  LoopVarEntry { loopVarStmDepth :: Int
               , loopVarType     :: IntType
               , loopVarBound    :: SubExp
               }

data LetBoundEntry lore =
  LetBoundEntry { letBoundDec      :: LetDec lore
                , letBoundAliases  :: Names
                , letBoundStm      :: Stm lore
                , letBoundStmDepth :: Int
                , letBoundIndex    :: Int -> IndexArray
                -- ^ Index a delayed array, if possible.
                , letBoundConsumed :: Bool
                  -- ^ True if consumed.
                }

data FParamEntry lore =
  FParamEntry { fparamDec      :: FParamInfo lore
              , fparamAliases  :: Names
              , fparamStmDepth :: Int
              , fparamConsumed :: Bool
              }

data LParamEntry lore =
  LParamEntry { lparamDec      :: LParamInfo lore
              , lparamStmDepth :: Int
              , lparamIndex    :: IndexArray
              , lparamConsumed :: Bool
              }

data FreeVarEntry lore =
  FreeVarEntry { freeVarDec      :: NameInfo lore
               , freeVarStmDepth :: Int
               , freeVarIndex    :: VName -> IndexArray
                -- ^ Index a delayed array, if possible.
               , freeVarConsumed :: Bool
                -- ^ True if consumed.
               }

entryInfo :: Entry lore -> NameInfo lore
entryInfo (LetBound entry) = LetName $ letBoundDec entry
entryInfo (LoopVar entry) = IndexName $ loopVarType entry
entryInfo (FParam entry) = FParamName $ fparamDec entry
entryInfo (LParam entry) = LParamName $ lparamDec entry
entryInfo (FreeVar entry) = freeVarDec entry

entryType :: ASTLore lore => Entry lore -> Type
entryType = typeOf . entryInfo

isVarBound :: Entry lore -> Maybe (LetBoundEntry lore)
isVarBound (LetBound entry) = Just entry
isVarBound _ = Nothing

bindingDepth :: Entry lore -> Int
bindingDepth (LetBound entry) = letBoundStmDepth entry
bindingDepth (FParam entry) = fparamStmDepth entry
bindingDepth (LParam entry) = lparamStmDepth entry
bindingDepth (LoopVar entry) = loopVarStmDepth entry
bindingDepth (FreeVar _) = 0

setStmDepth :: Int -> Entry lore -> Entry lore
setStmDepth d (LetBound entry) =
  LetBound $ entry { letBoundStmDepth = d }
setStmDepth d (FParam entry) =
  FParam $ entry { fparamStmDepth = d }
setStmDepth d (LParam entry) =
  LParam $ entry { lparamStmDepth = d }
setStmDepth d (LoopVar entry) =
  LoopVar $ entry { loopVarStmDepth = d }
setStmDepth d (FreeVar entry) =
  FreeVar $ entry { freeVarStmDepth = d }

consumed :: Entry lore -> Bool
consumed (LetBound entry) = letBoundConsumed entry
consumed (FParam entry)   = fparamConsumed entry
consumed (LParam entry)   = lparamConsumed entry
consumed LoopVar{}        = False
consumed (FreeVar entry)  = freeVarConsumed entry

entryStm :: Entry lore -> Maybe (Stm lore)
entryStm (LetBound entry) = Just $ letBoundStm entry
entryStm _                = Nothing

entryLetBoundDec :: Entry lore -> Maybe (LetDec lore)
entryLetBoundDec (LetBound entry) = Just $ letBoundDec entry
entryLetBoundDec _                = Nothing

asStm :: Entry lore -> Maybe (Stm lore)
asStm = fmap letBoundStm . isVarBound

elem :: VName -> SymbolTable lore -> Bool
elem name = isJust . lookup name

lookup :: VName -> SymbolTable lore -> Maybe (Entry lore)
lookup name = M.lookup name . bindings

lookupStm :: VName -> SymbolTable lore -> Maybe (Stm lore)
lookupStm name vtable = asStm =<< lookup name vtable

lookupExp :: VName -> SymbolTable lore -> Maybe (Exp lore, Certificates)
lookupExp name vtable = (stmExp &&& stmCerts) <$> lookupStm name vtable

lookupBasicOp :: VName -> SymbolTable lore -> Maybe (BasicOp, Certificates)
lookupBasicOp name vtable = case lookupExp name vtable of
  Just (BasicOp e, cs) -> Just (e, cs)
  _                    -> Nothing

lookupType :: ASTLore lore => VName -> SymbolTable lore -> Maybe Type
lookupType name vtable = entryType <$> lookup name vtable

lookupSubExpType :: ASTLore lore => SubExp -> SymbolTable lore -> Maybe Type
lookupSubExpType (Var v) = lookupType v
lookupSubExpType (Constant v) = const $ Just $ Prim $ primValueType v

lookupSubExp :: VName -> SymbolTable lore -> Maybe (SubExp, Certificates)
lookupSubExp name vtable = do
  (e,cs) <- lookupExp name vtable
  case e of
    BasicOp (SubExp se) -> Just (se,cs)
    _                   -> Nothing

lookupAliases :: VName -> SymbolTable lore -> Names
lookupAliases name vtable = case M.lookup name $ bindings vtable of
                              Just (LetBound e) -> letBoundAliases e
                              Just (FParam e)   -> fparamAliases e
                              _                 -> mempty

-- | If the given variable name is the name of a 'ForLoop' parameter,
-- then return the bound of that loop.
lookupLoopVar :: VName -> SymbolTable lore -> Maybe SubExp
lookupLoopVar name vtable = do
  LoopVar e <- M.lookup name $ bindings vtable
  return $ loopVarBound e

-- | In symbol table and not consumed.
available :: VName -> SymbolTable lore -> Bool
available name = maybe False (not . consumed) . M.lookup name . bindings

index :: ASTLore lore => VName -> [SubExp] -> SymbolTable lore
      -> Maybe Indexed
index name is table = do
  is' <- mapM asPrimExp is
  index' name is' table
  where asPrimExp i = do
          Prim t <- lookupSubExpType i table
          return $ primExpFromSubExp t i

index' :: VName -> [PrimExp VName] -> SymbolTable lore
       -> Maybe Indexed
index' name is vtable = do
  entry <- lookup name vtable
  case entry of
    LetBound entry' |
      Just k <- elemIndex name $ patternValueNames $
                stmPattern $ letBoundStm entry' ->
        letBoundIndex entry' k is
    FreeVar entry' ->
      freeVarIndex entry' name is
    LParam entry' -> lparamIndex entry' is
    _ -> Nothing

class IndexOp op where
  indexOp :: (ASTLore lore, IndexOp (Op lore)) =>
             SymbolTable lore -> Int -> op
          -> [PrimExp VName] -> Maybe Indexed
  indexOp _ _ _ _ = Nothing

instance IndexOp () where

indexExp :: (IndexOp (Op lore), ASTLore lore) =>
            SymbolTable lore -> Exp lore -> Int -> IndexArray

indexExp vtable (Op op) k is =
  indexOp vtable k op is

indexExp _ (BasicOp (Iota _ x s to_it)) _ [i]
  | IntType from_it <- primExpType i =
      Just $ Indexed mempty $
       ConvOpExp (SExt from_it to_it) i
       * primExpFromSubExp (IntType to_it) s
       + primExpFromSubExp (IntType to_it) x

indexExp table (BasicOp (Replicate (Shape ds) v)) _ is
  | length ds == length is,
    Just (Prim t) <- lookupSubExpType v table =
      Just $ Indexed mempty $ primExpFromSubExp t v

indexExp table (BasicOp (Replicate (Shape [_]) (Var v))) _ (_:is) =
  index' v is table

indexExp table (BasicOp (Reshape newshape v)) _ is
  | Just oldshape <- arrayDims <$> lookupType v table =
      let is' =
            reshapeIndex (map (primExpFromSubExp int32) oldshape)
                         (map (primExpFromSubExp int32) $ newDims newshape)
                         is
      in index' v is' table

indexExp table (BasicOp (Index v slice)) _ is =
  index' v (adjust slice is) table
  where adjust (DimFix j:js') is' =
          pe j : adjust js' is'
        adjust (DimSlice j _ s:js') (i:is') =
          let i_t_s = i * pe s
              j_p_i_t_s = pe j + i_t_s
          in j_p_i_t_s : adjust js' is'
        adjust _ _ = []

        pe = primExpFromSubExp (IntType Int32)

indexExp _ _ _ _ = Nothing

defBndEntry :: (ASTLore lore, IndexOp (Op lore)) =>
               SymbolTable lore
            -> PatElem lore
            -> Names
            -> Stm lore
            -> LetBoundEntry lore
defBndEntry vtable patElem als bnd =
  LetBoundEntry {
      letBoundDec = patElemDec patElem
    , letBoundAliases = als
    , letBoundStm = bnd
    , letBoundStmDepth = 0
    , letBoundIndex = \k -> fmap (indexedAddCerts (stmAuxCerts $ stmAux bnd)) .
                            indexExp vtable (stmExp bnd) k
    , letBoundConsumed = False
    }

bindingEntries :: (ASTLore lore, Aliases.Aliased lore, IndexOp (Op lore)) =>
                  Stm lore -> SymbolTable lore
               -> [LetBoundEntry lore]
bindingEntries bnd@(Let pat _ _) vtable = do
  pat_elem <- patternElements pat
  return $ defBndEntry vtable pat_elem (Aliases.aliasesOf pat_elem) bnd

insertEntry :: VName -> Entry lore -> SymbolTable lore
            -> SymbolTable lore
insertEntry name entry =
  insertEntries [(name,entry)]

insertEntries :: [(VName, Entry lore)] -> SymbolTable lore
              -> SymbolTable lore
insertEntries entries vtable =
  vtable { bindings = foldl insertWithDepth (bindings vtable) entries }
  where insertWithDepth bnds (name, entry) =
          let entry' = setStmDepth (loopDepth vtable) entry
          in M.insert name entry' bnds

insertStm :: (ASTLore lore, IndexOp (Op lore), Aliases.Aliased lore) =>
             Stm lore
          -> SymbolTable lore
          -> SymbolTable lore
insertStm stm vtable =
  flip (foldl' $ flip consume) (namesToList stm_consumed) $
  flip (foldl' addRevAliases) (patternElements $ stmPattern stm) $
  insertEntries (zip names $ map LetBound $ bindingEntries stm vtable) vtable
  where names = patternNames $ stmPattern stm
        adjustSeveral f = flip $ foldl' $ flip $ M.adjust f
        stm_consumed = expandAliases (Aliases.consumedInStm stm) vtable
        addRevAliases vtable' pe =
          vtable' { bindings = adjustSeveral update inedges $ bindings vtable' }
          where inedges = namesToList $ expandAliases (Aliases.aliasesOf pe) vtable'
                update (LetBound entry) =
                  LetBound entry
                  { letBoundAliases = oneName (patElemName pe) <> letBoundAliases entry }
                update (FParam entry) =
                  FParam entry
                  { fparamAliases = oneName (patElemName pe) <> fparamAliases entry }
                update e = e

insertStms :: (ASTLore lore, IndexOp (Op lore), Aliases.Aliased lore) =>
              Stms lore
           -> SymbolTable lore
           -> SymbolTable lore
insertStms stms vtable = foldl' (flip insertStm) vtable $ stmsToList stms

expandAliases :: Names -> SymbolTable lore -> Names
expandAliases names vtable = names <> aliasesOfAliases
  where aliasesOfAliases =
          mconcat . map (`lookupAliases` vtable) . namesToList $ names

insertFParam :: AST.FParam lore
             -> SymbolTable lore
             -> SymbolTable lore
insertFParam fparam = insertEntry name entry
  where name = AST.paramName fparam
        entry = FParam FParamEntry { fparamDec = AST.paramDec fparam
                                   , fparamAliases = mempty
                                   , fparamStmDepth = 0
                                   , fparamConsumed = False
                                   }

insertFParams :: [AST.FParam lore]
              -> SymbolTable lore
              -> SymbolTable lore
insertFParams fparams symtable = foldl' (flip insertFParam) symtable fparams

insertLParamWithArray :: LParam lore -> IndexArray -> SymbolTable lore
                      -> SymbolTable lore
insertLParamWithArray param indexf = insertEntry name bind
  where bind = LParam LParamEntry { lparamDec = AST.paramDec param
                                  , lparamStmDepth = 0
                                  , lparamIndex = indexf
                                  , lparamConsumed = False
                                  }
        name = AST.paramName param

insertLParam :: LParam lore -> SymbolTable lore -> SymbolTable lore
insertLParam param = insertLParamWithArray param (const Nothing)

insertLoopVar :: VName -> IntType -> SubExp -> SymbolTable lore -> SymbolTable lore
insertLoopVar name it bound = insertEntry name bind
  where bind = LoopVar LoopVarEntry {
            loopVarStmDepth = 0
          , loopVarType = it
          , loopVarBound = bound
          }

insertFreeVar :: VName -> NameInfo lore -> SymbolTable lore -> SymbolTable lore
insertFreeVar name dec = insertEntry name entry
  where entry = FreeVar FreeVarEntry {
            freeVarDec = dec
          , freeVarStmDepth = 0
          , freeVarIndex  = \_ _ -> Nothing
          , freeVarConsumed = False
          }

consume :: VName -> SymbolTable lore -> SymbolTable lore
consume consumee vtable = foldl' consume' vtable $ namesToList $
                          expandAliases (oneName consumee) vtable
  where consume' vtable' v | Just e <- lookup v vtable = insertEntry v (consume'' e) vtable'
                           | otherwise                 = vtable'
        consume'' (FreeVar e)  = FreeVar e { freeVarConsumed = True }
        consume'' (LetBound e) = LetBound e { letBoundConsumed = True }
        consume'' (FParam e)   = FParam e { fparamConsumed = True }
        consume'' (LParam e)   = LParam e { lparamConsumed = True }
        consume'' (LoopVar e)  = LoopVar e

-- | Hide definitions of those entries that satisfy some predicate.
hideIf :: (Entry lore -> Bool) -> SymbolTable lore -> SymbolTable lore
hideIf hide vtable = vtable { bindings = M.map maybeHide $ bindings vtable }
  where maybeHide entry
          | hide entry = FreeVar FreeVarEntry { freeVarDec = entryInfo entry
                                              , freeVarStmDepth = bindingDepth entry
                                              , freeVarIndex = \_ _ -> Nothing
                                              , freeVarConsumed = consumed entry
                                              }
          | otherwise = entry

-- | Hide these definitions, if they are protected by certificates in
-- the set of names.
hideCertified :: Names -> SymbolTable lore -> SymbolTable lore
hideCertified to_hide = hideIf $ maybe False hide . entryStm
  where hide = any (`nameIn` to_hide) . unCertificates . stmCerts
