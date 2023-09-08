{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.AccessPattern
  ( analyzeMemoryAccessPatterns,
    analyseStm,
    ArrayIndexDescriptors,
    Variance,
    IterationType,
  )
where

import Data.Foldable
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Debug.Pretty.Simple
import Futhark.IR.GPU
import Futhark.Util.Pretty

-- | Iteration type describes whether the index is iterated in a parallel or
-- sequential way, ie. if the index expression comes from a sequential or
-- parallel construct, like foldl or map.
data IterationType = Sequential | Parallel
  deriving (Eq, Ord, Show)

-- | Variance represents whether the index is variant, or invariant to the outer
-- kernel/iteration function.
data Variance
  = Variant IterationType
  | Invariant
  deriving (Eq, Ord, Show)

-- | DimFix'es can either be a constant expression (IntValue) or a VName to some
-- pattern.
newtype DimIndexExpression = DimIndexExpression (Either VName PrimValue)
  deriving (Eq, Ord, Show)

-- | Collect all features of memory access together
data MemoryAccessPattern = MemoryAccessPattern
  { -- | Expression reference that is used to index into a given dimension
    dimIdxExpr :: DimIndexExpression,
    variance :: Variance
  }
  deriving (Eq, Ord, Show)

-- | Each element in the list corresponds to a dimension in the given array
type MemoryEntry = [MemoryAccessPattern]

-- | We map variable names of arrays to lists of memory access patterns.
type ArrayIndexDescriptors = M.Map VName [MemoryEntry]

-- | Map `Pat` to array index descriptors
type StmtsAids = M.Map VName ArrayIndexDescriptors

-- | Map Entries to array index descriptors (mostly used for debugging)
type FunAids = M.Map Name StmtsAids

type AnalyzeCtx = M.Map VName (IterationType, BasicOp)

-- | For each `entry` we return a tuple of (function-name and AIDs)
analyzeMemoryAccessPatterns :: Prog GPU -> FunAids
analyzeMemoryAccessPatterns prog =
  -- We map over the program functions (usually always entries)
  -- Then fold them together to a singular map.
  -- foldl' mergeAids M.empty .
  foldl' M.union M.empty $ getAids <$> progFuns prog

getAids :: FunDef GPU -> FunAids
getAids f = M.singleton fdname aids
  where
    fdname = funDefName f
    aids =
      (\p -> analyseStm p M.empty Sequential)
        -- functionBody -> [stm]
        . stmsToList
        . bodyStms
        . funDefBody
        $ f

-- | TODO FIXME PLZ
sizeOpToCnst :: SizeOp -> BasicOp
sizeOpToCnst = const undefined

-- | Extends the current analyzation context by mapping all input {patterns => basicop}
-- This is more often than not just a single pattern element.
extendCtx :: AnalyzeCtx -> [PatElem (LetDec GPU)] -> (IterationType, BasicOp) -> AnalyzeCtx
extendCtx ctx patterns =
  M.union ctx
    . M.fromList
    . zip (map patElemName patterns)
    . replicate (length patterns)

type ExpressionAnalyzer op = op -> AnalyzeCtx -> ArrayIndexDescriptors

analyzeExpression :: ExpressionAnalyzer op -> [PatElem (LetDec GPU)] -> (op -> AnalyzeCtx -> StmtsAids)
analyzeExpression f pp o ctx =
  M.fromList
    . zip (map patElemName pp)
    . replicate (length pp) -- get vnames of patterns
    $ f o ctx -- create a result for each pattern

analyseStm :: [Stm GPU] -> AnalyzeCtx -> IterationType -> StmtsAids
analyseStm (stm : ss) ctx it =
  case stm of
    (Let (Pat pp) _ (BasicOp o)) ->
      let ctx' = extendCtx ctx pp (it, o)
       in let res = analyzeExpression analyseOp pp o ctx
           in M.union res $ analyseStm ss ctx' it
    (Let (Pat pp) _ (Op (SegOp o))) ->
      let res = analyzeExpression analyseKernelBody pp o ctx
       in M.union res $ analyseStm ss ctx it
    (Let (Pat pp) _ (Op (SizeOp o))) ->
      let ctx' = extendCtx ctx pp (it, sizeOpToCnst o)
       in analyseStm ss ctx' it
    -- TODO: Add patterns here. ( which ones? )
    -- OtherOp (op rep)
    (Let (Pat pp) _ (Op (GPUBody _ b))) ->
      -- TODO: Add to context ((somehow))
      let res' =
            foldl (M.unionWith (++)) M.empty
              . map snd
              . M.toList
              -- GPUBody is sequential!
              . (\p -> analyseStm p M.empty Sequential)
              . stmsToList
              $ bodyStms b
       in let res =
                M.fromList
                  . zip (map patElemName pp)
                  . replicate (length pp) -- get vnames of patterns
                  $ res'
           in -- in let ctx' = extendCtx ctx pp (it, o)
              M.union res $ analyseStm ss ctx it
    (Let (Pat _pp) _ _o) ->
      analyseStm ss ctx it
analyseStm [] c _ = M.empty

-- | Extend current `ctx` with segSpace defs, as parallel
analyseKernelBody :: SegOp SegLevel GPU -> AnalyzeCtx -> ArrayIndexDescriptors
analyseKernelBody op ctx =
  let ctx' = M.union ctx . M.fromList . map toCtx . unSegSpace $ segSpace op
   in analyseOpStm ctx' . stmsToList . kernelBodyStms $ segBody op
  where
    toCtx (n, o) = (n, (Parallel, SubExp o))
    analyseOpStm :: AnalyzeCtx -> [Stm GPU] -> ArrayIndexDescriptors
    analyseOpStm ctx' prog =
      foldl (M.unionWith (++)) M.empty . toList $ analyseStm prog ctx' Parallel

-- | Construct MemoryAccessPattern using a `VName`
accesssPatternOfVName :: VName -> Variance -> MemoryAccessPattern
accesssPatternOfVName n =
  MemoryAccessPattern $ DimIndexExpression $ Left n

-- | Construct MemoryAccessPattern using a `PrimValue`
accesssPatternOfPrimValue :: PrimValue -> MemoryAccessPattern
accesssPatternOfPrimValue n =
  MemoryAccessPattern (DimIndexExpression $ Right n) Invariant

-- | Return the map of all access from a given BasicOperator
-- TODO: Implement other BasicOp patterns than Index
-- In reality, we should use the Maybe monad, instead of returning empty maps,
-- but i am lazy and we have a deadline.
analyseOp :: BasicOp -> AnalyzeCtx -> ArrayIndexDescriptors
analyseOp (Index name (Slice unslice)) m =
  M.singleton name $
    L.singleton $
      map
        ( \case
            (DimFix (Constant primvalue)) ->
              -- accesssPatternOfVName (VName "eatCraaaap" 6969) (Variant Sequential)
              accesssPatternOfPrimValue primvalue
            (DimFix (Var name')) ->
              case getOpVariance m . snd $ fromJust $ M.lookup name' m of
                Nothing -> accesssPatternOfVName name' Invariant
                Just v -> accesssPatternOfVName name' v
            (DimSlice {}) -> accesssPatternOfVName (VName "NicerSlicer" 42) (Variant Sequential)
            _ -> accesssPatternOfVName (VName "UnhandledCase" 43) (Variant Sequential)
        )
        unslice
analyseOp (BinOp {}) _ = M.empty
analyseOp _ _ = M.empty

getSubExpVariance :: AnalyzeCtx -> SubExp -> Maybe Variance
getSubExpVariance _ (Constant _) = Just Invariant
getSubExpVariance c (Var vname) =
  case M.lookup vname c of
    Nothing -> Just Invariant
    -- TODO: Is this the right way to do it?
    --  All let p = .. are marked as parallel inside kernel bodies
    --  The gtid is added to context with parallel, how do we distinguish them?
    Just (Parallel, _) -> Just $ Variant Parallel
    Just op -> getOpVariance c $ snd op

-- | Combine two `Maybe Variance`s into the worst-case variance.
-- This is used to reduce expressions consisting of multiple sub-expressions.
(><) :: Maybe Variance -> Maybe Variance -> Maybe Variance
(><) Nothing rhs = rhs
(><) lhs Nothing = lhs
(><) (Just lhs) (Just rhs) =
  case (lhs, rhs) of
    -- If either is variant, the expression is variant
    (Invariant, Variant i) -> Just $ Variant i
    (Variant i, Invariant) -> Just $ Variant i
    -- If both is invariant, the expression is variant with the worst-case iter type (Parallel)
    (Variant i, Variant j) ->
      Just $
        if i == Parallel
          then Variant Parallel
          else Variant j
    (Invariant, Invariant) -> Just Invariant

-- | Get variance from Basic operators. Looks up variables in the current
-- context to determine whether an operator should be marked `Invariant`, or
-- `Variant` with some additional information.
getOpVariance :: AnalyzeCtx -> BasicOp -> Maybe Variance
getOpVariance c (SubExp e) = getSubExpVariance c e
getOpVariance _ (ArrayLit (_e : _ee) _) = Nothing
getOpVariance c (UnOp _ e) = getSubExpVariance c e
getOpVariance c (BinOp _ l r) = getSubExpVariance c l >< getSubExpVariance c r
getOpVariance c (CmpOp _ l r) = getSubExpVariance c l >< getSubExpVariance c r
getOpVariance c (ConvOp _ e) = getSubExpVariance c e
getOpVariance c (Assert e _ _) = getSubExpVariance c e
-- Usually hit in indirect indices
getOpVariance c (Index _name (Slice ee)) = foldl (><) Nothing $ map (getSubExpVariance c . fromJust . dimFix) ee
--
getOpVariance c (Update _ _name (Slice dsts) _src) = getSubExpVariance c . fromJust . dimFix $ head dsts
getOpVariance c (FlatIndex _name (FlatSlice e _dims)) = getSubExpVariance c e
getOpVariance c (FlatUpdate _name1 (FlatSlice e _dims) _name2) = getSubExpVariance c e
-- TODO: Continue from src/Futhark/IR/Syntax.hs:349

getOpVariance _ _ = Nothing

-- Pretty printing stuffs
-- TODO: Reorder this to an order that makes a little more sense
instance Pretty FunAids where
  pretty = stack . map f . M.toList :: FunAids -> Doc ann
    where
      f (entryName, aids) = pretty entryName </> indent 2 (pretty aids)

instance Pretty StmtsAids where
  pretty = stack . map f . M.toList :: StmtsAids -> Doc ann
    where
      f (stmtPat, aids) =
        if null aids
          then pretty stmtPat <+> "=> []"
          else pretty stmtPat <+> "=> [" </> indent 2 (pretty aids) </> "]"

instance Pretty ArrayIndexDescriptors where
  pretty = stack . map f . M.toList :: ArrayIndexDescriptors -> Doc ann
    where
      mapprint [] = ""
      mapprint [e] = memoryEntryPrint e
      mapprint (e : ee) = memoryEntryPrint e </> mapprint ee

      memoryEntryPrint = hsep . punctuate " " . map pretty
      f (n, maps) = pretty n </> indent 2 (mapprint maps)

instance Pretty MemoryAccessPattern where
  pretty (MemoryAccessPattern d v) =
    -- Instead of using `brackets $` we manually enclose with `[`s, to add
    -- spacing between the enclosed elements
    "[" <+> pretty d <+> "|" <+> pretty v <+> "]"

instance Pretty DimIndexExpression where
  pretty (DimIndexExpression (Left n)) = "σ" <+> pretty n -- sigma since it exists in our store
  pretty (DimIndexExpression (Right c)) = "τ" <+> pretty c -- tau for a term

instance Pretty IterationType where
  pretty Sequential = "seq"
  pretty Parallel = "par"

instance Pretty Variance where
  pretty (Variant t) = "ν" <+> pretty t -- v for variant
  pretty Invariant = "ψ" -- v dashed for invariant
