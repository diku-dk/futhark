{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Analysis.SymbolTable
  ( SymbolTable (bindings, loopDepth, availableAtClosestLoop, simplifyMemory)
  , empty
  , fromScope
  , toScope
  , castSymbolTable
    -- * Entries
  , Entry
  , deepen
  , bindingDepth
  , valueRange
  , loopVariable
  , entryStm
  , entryLetBoundAttr
  , entryFParamLore
  , entryType
  , asScalExp
    -- * Lookup
  , elem
  , lookup
  , lookupStm
  , lookupExp
  , lookupBasicOp
  , lookupType
  , lookupSubExp
  , lookupScalExp
  , lookupValue
  , lookupVar
  , lookupAliases
  , available
  , consume
  , index
  , index'
  , Indexed(..)
  , indexedAddCerts
  , IndexOp(..)
    -- * Insertion
  , insertStm
  , insertFParams
  , insertLParam
  , insertArrayLParam
  , insertChunkLParam
  , insertLoopVar
    -- * Bounds
  , updateBounds
  , setUpperBound
  , setLowerBound
  , isAtLeast
    -- * Misc
  , enclosingLoopVars
  , rangesRep
  , hideIf
  , hideCertified
  )
  where

import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Reader
import Data.Ord
import Data.Maybe
import Data.List hiding (elem, lookup)
import qualified Data.List as L
import qualified Data.Set        as S
import qualified Data.Map.Strict as M

import Prelude hiding (elem, lookup)

import Futhark.Analysis.PrimExp.Convert
import Futhark.Representation.AST hiding (FParam, lookupType)
import qualified Futhark.Representation.AST as AST
import Futhark.Analysis.ScalExp

import Futhark.Analysis.Rephrase
import qualified Futhark.Analysis.AlgSimplify as AS
import Futhark.Representation.AST.Attributes.Ranges
  (Range, ScalExpRange, Ranged)
import qualified Futhark.Representation.AST.Attributes.Ranges as Ranges
import qualified Futhark.Representation.AST.Attributes.Aliases as Aliases

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

fromScope :: Attributes lore => Scope lore -> SymbolTable lore
fromScope = M.foldlWithKey' insertFreeVar' empty
  where insertFreeVar' m k attr = insertFreeVar k attr m

toScope :: SymbolTable lore -> Scope lore
toScope = M.map entryInfo . bindings

-- | Try to convert a symbol table for one representation into a
-- symbol table for another.  The two symbol tables will have the same
-- keys, but some entries may be diferent (i.e. some expression
-- entries will have been turned into free variable entries).
castSymbolTable :: (SameScope from to,
                    ExpAttr from ~ ExpAttr to,
                    BodyAttr from ~ BodyAttr to,
                    RetType from ~ RetType to,
                    BranchType from ~ BranchType to) =>
                   SymbolTable from -> SymbolTable to
castSymbolTable table = genCastSymbolTable loopVar letBound fParam lParam freeVar table
  where loopVar (LoopVarEntry r d it) = LoopVar $ LoopVarEntry r d it
        letBound e
          | Just e' <- castStm $ letBoundStm e =
              LetBound e { letBoundStm = e'
                         , letBoundAttr = letBoundAttr e
                         }
          | otherwise =
              FreeVar FreeVarEntry
              { freeVarAttr = LetInfo $ letBoundAttr e
              , freeVarStmDepth = letBoundStmDepth e
              , freeVarRange = letBoundRange e
              , freeVarIndex = \name is -> index' name is table
              , freeVarConsumed = letBoundConsumed e
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
genCastSymbolTable loopVar letBound fParam lParam freeVar vtable =
  vtable { bindings = M.map onEntry $ bindings vtable }
  where onEntry (LoopVar entry) = loopVar entry
        onEntry (LetBound entry) = letBound entry
        onEntry (FParam entry) = fParam entry
        onEntry (LParam entry) = lParam entry
        onEntry (FreeVar entry) = freeVar entry

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
  LoopVarEntry { loopVarRange    :: ScalExpRange
               , loopVarStmDepth :: Int
               , loopVarType     :: IntType
               }

data LetBoundEntry lore =
  LetBoundEntry { letBoundRange    :: ScalExpRange
                , letBoundAttr     :: LetAttr lore
                , letBoundAliases  :: Names
                , letBoundStm      :: Stm lore
                , letBoundStmDepth :: Int
                , letBoundScalExp  :: Maybe ScalExp
                , letBoundIndex    :: Int -> IndexArray
                -- ^ Index a delayed array, if possible.
                , letBoundConsumed :: Bool
                  -- ^ True if consumed.
                }

data FParamEntry lore =
  FParamEntry { fparamRange    :: ScalExpRange
              , fparamAttr     :: FParamAttr lore
              , fparamAliases  :: Names
              , fparamStmDepth :: Int
              , fparamConsumed :: Bool
              }

data LParamEntry lore =
  LParamEntry { lparamRange    :: ScalExpRange
              , lparamAttr     :: LParamAttr lore
              , lparamStmDepth :: Int
              , lparamIndex    :: IndexArray
              , lparamConsumed :: Bool
              }

data FreeVarEntry lore =
  FreeVarEntry { freeVarAttr     :: NameInfo lore
               , freeVarStmDepth :: Int
               , freeVarRange    :: ScalExpRange
               , freeVarIndex    :: VName -> IndexArray
                -- ^ Index a delayed array, if possible.
               , freeVarConsumed :: Bool
                -- ^ True if consumed.
               }

entryInfo :: Entry lore -> NameInfo lore
entryInfo (LetBound entry) = LetInfo $ letBoundAttr entry
entryInfo (LoopVar entry) = IndexInfo $ loopVarType entry
entryInfo (FParam entry) = FParamInfo $ fparamAttr entry
entryInfo (LParam entry) = LParamInfo $ lparamAttr entry
entryInfo (FreeVar entry) = freeVarAttr entry

entryType :: Attributes lore => Entry lore -> Type
entryType = typeOf . entryInfo

isVarBound :: Entry lore -> Maybe (LetBoundEntry lore)
isVarBound (LetBound entry) = Just entry
isVarBound _ = Nothing

asScalExp :: Entry lore -> Maybe ScalExp
asScalExp = letBoundScalExp <=< isVarBound

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

consumed :: Entry lore -> Bool
consumed (LetBound entry) = letBoundConsumed entry
consumed (FParam entry)   = fparamConsumed entry
consumed (LParam entry)   = lparamConsumed entry
consumed LoopVar{}        = False
consumed (FreeVar entry)  = freeVarConsumed entry

entryStm :: Entry lore -> Maybe (Stm lore)
entryStm (LetBound entry) = Just $ letBoundStm entry
entryStm _                = Nothing

entryLetBoundAttr :: Entry lore -> Maybe (LetAttr lore)
entryLetBoundAttr (LetBound entry) = Just $ letBoundAttr entry
entryLetBoundAttr _                = Nothing

entryFParamLore :: Entry lore -> Maybe (FParamAttr lore)
entryFParamLore (FParam entry) = Just $ fparamAttr entry
entryFParamLore _              = Nothing

loopVariable :: Entry lore -> Bool
loopVariable (LoopVar _) = True
loopVariable _           = False

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

lookupBasicOp :: VName -> SymbolTable lore -> Maybe (BasicOp lore, Certificates)
lookupBasicOp name vtable = case lookupExp name vtable of
  Just (BasicOp e, cs) -> Just (e, cs)
  _                    -> Nothing

lookupType :: Attributes lore => VName -> SymbolTable lore -> Maybe Type
lookupType name vtable = entryType <$> lookup name vtable

lookupSubExpType :: Attributes lore => SubExp -> SymbolTable lore -> Maybe Type
lookupSubExpType (Var v) = lookupType v
lookupSubExpType (Constant v) = const $ Just $ Prim $ primValueType v

lookupSubExp :: VName -> SymbolTable lore -> Maybe (SubExp, Certificates)
lookupSubExp name vtable = do
  (e,cs) <- lookupExp name vtable
  case e of
    BasicOp (SubExp se) -> Just (se,cs)
    _                   -> Nothing

lookupScalExp :: Attributes lore => VName -> SymbolTable lore -> Maybe ScalExp
lookupScalExp name vtable =
  case (lookup name vtable, lookupRange name vtable) of
    -- If we know the lower and upper bound, and these are the same,
    -- then we morally know the ScalExp, but only if the variable has
    -- the right type.
    (Just entry, (Just lower, Just upper))
      | entryType entry == Prim int32,
        lower == upper, scalExpType lower == int32 ->
          Just $ expandScalExp (`lookupScalExp` vtable) lower
    (Just entry, _) -> asScalExp entry
    _ -> Nothing

lookupValue :: VName -> SymbolTable lore -> Maybe (PrimValue, Certificates)
lookupValue name vtable = case lookupSubExp name vtable of
                            Just (Constant val, cs) -> Just (val, cs)
                            _                       -> Nothing

lookupVar :: VName -> SymbolTable lore -> Maybe (VName, Certificates)
lookupVar name vtable = case lookupSubExp name vtable of
                          Just (Var v, cs) -> Just (v, cs)
                          _                -> Nothing

lookupAliases :: VName -> SymbolTable lore -> Names
lookupAliases name vtable = case M.lookup name $ bindings vtable of
                              Just (LetBound e) -> letBoundAliases e
                              Just (FParam e)   -> fparamAliases e
                              _                 -> mempty

-- | In symbol table and not consumed.
available :: VName -> SymbolTable lore -> Bool
available name = maybe False (not . consumed) . M.lookup name . bindings

index :: Attributes lore => VName -> [SubExp] -> SymbolTable lore
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
rangesRep = M.filter knownRange . M.map toRep . bindings
  where toRep entry = (bindingDepth entry, lower, upper)
          where (lower, upper) = valueRange entry
        knownRange (_, lower, upper) = isJust lower || isJust upper

class IndexOp op where
  indexOp :: (Attributes lore, IndexOp (Op lore)) =>
             SymbolTable lore -> Int -> op
          -> [PrimExp VName] -> Maybe Indexed
  indexOp _ _ _ _ = Nothing

instance IndexOp () where

indexExp :: (IndexOp (Op lore), Attributes lore) =>
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

indexChunk :: SymbolTable lore -> VName -> VName -> IndexArray
indexChunk table offset array (i:is) =
  index' array (offset'+i:is) table
  where offset' = primExpFromSubExp (IntType Int32) (Var offset)
indexChunk _ _ _ _ = Nothing

defBndEntry :: (Attributes lore, IndexOp (Op lore)) =>
               SymbolTable lore
            -> PatElem lore
            -> Range
            -> Names
            -> Stm lore
            -> LetBoundEntry lore
defBndEntry vtable patElem range als bnd =
  LetBoundEntry {
      letBoundRange = simplifyRange $ scalExpRange range
    , letBoundAttr = patElemAttr patElem
    , letBoundAliases = als
    , letBoundStm = bnd
    , letBoundScalExp =
      runReader (toScalExp (`lookupScalExp` vtable) (stmExp bnd)) types
    , letBoundStmDepth = 0
    , letBoundIndex = \k -> fmap (indexedAddCerts (stmAuxCerts $ stmAux bnd)) .
                            indexExp vtable (stmExp bnd) k
    , letBoundConsumed = False
    }
  where ranges :: AS.RangesRep
        ranges = rangesRep vtable

        types = toScope vtable

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

        simplifyBound (Just se) | scalExpType se == int32 =
          Just $ AS.simplify se ranges
        simplifyBound _ =
          Nothing

bindingEntries :: (Ranged lore, Aliases.Aliased lore, IndexOp (Op lore)) =>
                  Stm lore -> SymbolTable lore
               -> [LetBoundEntry lore]
bindingEntries bnd@(Let pat _ _) vtable = do
  pat_elem <- patternElements pat
  return $ defBndEntry vtable pat_elem
    (Ranges.rangeOf pat_elem) (Aliases.aliasesOf pat_elem) bnd

insertEntry :: Attributes lore =>
               VName -> Entry lore -> SymbolTable lore
            -> SymbolTable lore
insertEntry name entry =
  insertEntries [(name,entry)]

insertEntries :: Attributes lore =>
                 [(VName, Entry lore)] -> SymbolTable lore
              -> SymbolTable lore
insertEntries entries vtable =
  let vtable' = vtable { bindings = foldl insertWithDepth (bindings vtable) entries }
  in foldr (`isAtLeast` 0) vtable' dim_vars
  where insertWithDepth bnds (name, entry) =
          let entry' = setStmDepth (loopDepth vtable) entry
          in M.insert name entry' bnds
        dim_vars = subExpVars $ concatMap (arrayDims . entryType . snd) entries

insertStm :: (IndexOp (Op lore), Ranged lore, Aliases.Aliased lore) =>
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

expandAliases :: Names -> SymbolTable lore -> Names
expandAliases names vtable = names <> aliasesOfAliases
  where aliasesOfAliases =
          mconcat . map (`lookupAliases` vtable) . namesToList $ names

insertFParam :: Attributes lore =>
                AST.FParam lore
             -> SymbolTable lore
             -> SymbolTable lore
insertFParam fparam = flip (foldr (`isAtLeast` 0)) sizes . insertEntry name entry
  where name = AST.paramName fparam
        entry = FParam FParamEntry { fparamRange = (Nothing, Nothing)
                                   , fparamAttr = AST.paramAttr fparam
                                   , fparamAliases = mempty
                                   , fparamStmDepth = 0
                                   , fparamConsumed = False
                                   }
        sizes = subExpVars $ arrayDims $ AST.paramType fparam

insertFParams :: Attributes lore =>
                 [AST.FParam lore]
              -> SymbolTable lore
              -> SymbolTable lore
insertFParams fparams symtable = foldl' (flip insertFParam) symtable fparams

insertLParamWithRange :: Attributes lore =>
                         LParam lore -> ScalExpRange -> IndexArray -> SymbolTable lore
                      -> SymbolTable lore
insertLParamWithRange param range indexf vtable =
  -- We know that the sizes in the type of param are at least zero,
  -- since they are array sizes.
  let vtable' = insertEntry name bind vtable
  in foldr (`isAtLeast` 0) vtable' sizevars
  where bind = LParam LParamEntry { lparamRange = range
                                  , lparamAttr = AST.paramAttr param
                                  , lparamStmDepth = 0
                                  , lparamIndex = indexf
                                  , lparamConsumed = False
                                  }
        name = AST.paramName param
        sizevars = subExpVars $ arrayDims $ AST.paramType param

insertLParam :: Attributes lore =>
                LParam lore -> SymbolTable lore -> SymbolTable lore
insertLParam param =
  insertLParamWithRange param (Nothing, Nothing) (const Nothing)

insertArrayLParam :: Attributes lore =>
                     LParam lore -> Maybe VName -> SymbolTable lore
                  -> SymbolTable lore
insertArrayLParam param (Just array) vtable =
  -- We now know that the outer size of 'array' is at least one, and
  -- that the inner sizes are at least zero, since they are array
  -- sizes.
  let vtable' = insertLParamWithRange param (lookupRange array vtable) (const Nothing) vtable
  in case arrayDims <$> lookupType array vtable of
    Just (Var v:_) -> (v `isAtLeast` 1) vtable'
    _              -> vtable'
insertArrayLParam param Nothing vtable =
  -- Well, we still know that it's a param...
  insertLParam param vtable

insertChunkLParam :: Attributes lore =>
                     VName -> LParam lore -> VName -> SymbolTable lore
                  -> SymbolTable lore
insertChunkLParam offset param array vtable =
  -- We now know that the outer size of 'array' is at least one, and
  -- that the inner sizes are at least zero, since they are array
  -- sizes.
  let vtable' = insertLParamWithRange param
                (lookupRange array vtable) (indexChunk vtable offset array) vtable
  in case arrayDims <$> lookupType array vtable of
    Just (Var v:_) -> (v `isAtLeast` 1) vtable'
    _              -> vtable'

insertLoopVar :: Attributes lore => VName -> IntType -> SubExp -> SymbolTable lore -> SymbolTable lore
insertLoopVar name it bound = insertEntry name bind
  where bind = LoopVar LoopVarEntry {
            loopVarRange = (Just 0,
                            Just $ subExpToScalExp bound (IntType it) - 1)
          , loopVarStmDepth = 0
          , loopVarType = it
          }

insertFreeVar :: Attributes lore => VName -> NameInfo lore -> SymbolTable lore -> SymbolTable lore
insertFreeVar name attr = insertEntry name entry
  where entry = FreeVar FreeVarEntry {
            freeVarAttr = attr
          , freeVarRange = (Nothing, Nothing)
          , freeVarStmDepth = 0
          , freeVarIndex  = \_ _ -> Nothing
          , freeVarConsumed = False
          }

updateBounds :: Attributes lore => Bool -> SubExp -> SymbolTable lore -> SymbolTable lore
updateBounds isTrue cond vtable =
  case runReader (toScalExp (`lookupScalExp` vtable) $ BasicOp $ SubExp cond) types of
    Nothing    -> vtable
    Just cond' ->
      let cond'' | isTrue    = cond'
                 | otherwise = SNot cond'
      in updateBounds' cond'' vtable
  where types = toScope vtable

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

      ranges = M.filter nonEmptyRange $ M.map toRep $ bindings sym_tab
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
            Just sy -> let trclsyms = foldl trClSymsInRange mempty $ namesToList $
                                      candidates `namesSubtract` oneName sy
                       in  if   sy `nameIn` trclsyms
                           then pickRefinedSym (S.insert sy elsyms0) e_scal
                           else sym0
            Nothing -> sym0

      -- computes the transitive closure of the symbols appearing
      -- in the ranges of a symbol
      trClSymsInRange :: Names -> VName -> Names
      trClSymsInRange cur_syms sym =
        if sym `nameIn` cur_syms then cur_syms
        else case M.lookup sym ranges of
               Just (_,lb,ub) -> let sym_bds = concatMap (namesToList . freeIn) (catMaybes [lb, ub])
                                 in  foldl trClSymsInRange
                                           (oneName sym <> cur_syms)
                                           sym_bds
               Nothing        -> oneName sym <> cur_syms

consume :: Attributes lore => VName -> SymbolTable lore -> SymbolTable lore
consume consumee vtable = foldl' consume' vtable $ namesToList $
                          expandAliases (oneName consumee) vtable
  where consume' vtable' v | Just e <- lookup v vtable = insertEntry v (consume'' e) vtable'
                           | otherwise                 = vtable'
        consume'' (FreeVar e)  = FreeVar e { freeVarConsumed = True }
        consume'' (LetBound e) = LetBound e { letBoundConsumed = True }
        consume'' (FParam e)   = FParam e { fparamConsumed = True }
        consume'' (LParam e)   = LParam e { lparamConsumed = True }
        consume'' (LoopVar e)  = LoopVar e

setUpperBound :: VName -> ScalExp -> SymbolTable lore
              -> SymbolTable lore
setUpperBound name bound vtable =
  vtable { bindings = M.adjust setUpperBound' name $ bindings vtable }
  where setUpperBound' entry =
          let (oldLowerBound, oldUpperBound) = valueRange entry
          in if alreadyTheBound bound True oldUpperBound
             then entry
             else setValueRange
                  (oldLowerBound,
                   Just $ maybe bound (MaxMin True . (:[bound])) oldUpperBound)
                  entry

setLowerBound :: VName -> ScalExp -> SymbolTable lore -> SymbolTable lore
setLowerBound name bound vtable =
  vtable { bindings = M.adjust setLowerBound' name $ bindings vtable }
  where setLowerBound' entry =
          let (oldLowerBound, oldUpperBound) = valueRange entry
          in if alreadyTheBound bound False oldLowerBound
             then entry
             else setValueRange
                  (Just $ maybe bound (MaxMin False . (:[bound])) oldLowerBound,
                   oldUpperBound)
                  entry

alreadyTheBound :: ScalExp -> Bool -> Maybe ScalExp -> Bool
alreadyTheBound _ _ Nothing = False
alreadyTheBound new_bound b1 (Just cur_bound)
  | cur_bound == new_bound = True
  | MaxMin b2 ses <- cur_bound = b1 == b2 && (new_bound `L.elem` ses)
  | otherwise = False

isAtLeast :: VName -> Int -> SymbolTable lore -> SymbolTable lore
isAtLeast name x =
  setLowerBound name $ fromIntegral x

-- | Hide definitions of those entries that satisfy some predicate.
hideIf :: (Entry lore -> Bool) -> SymbolTable lore -> SymbolTable lore
hideIf hide vtable = vtable { bindings = M.map maybeHide $ bindings vtable }
  where maybeHide entry
          | hide entry = FreeVar FreeVarEntry { freeVarAttr = entryInfo entry
                                              , freeVarStmDepth = bindingDepth entry
                                              , freeVarRange = valueRange entry
                                              , freeVarIndex = \_ _ -> Nothing
                                              , freeVarConsumed = consumed entry
                                              }
          | otherwise = entry

-- | Hide these definitions, if they are protected by certificates in
-- the set of names.
hideCertified :: Names -> SymbolTable lore -> SymbolTable lore
hideCertified to_hide = hideIf $ maybe False hide . entryStm
  where hide = any (`nameIn` to_hide) . unCertificates . stmCerts
