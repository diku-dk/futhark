{-# LANGUAGE LambdaCase #-}

module Futhark.Analysis.AccessPattern
  ( analyzeMemoryAccessPatterns,
    ArrayIndexDescriptors,
    Variance,
    IterationType,
  )
where

import Data.Foldable
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
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
      (\p -> analyseStms p M.empty Sequential)
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

type ExpressionAnalyzer op = op -> AnalyzeCtx -> Maybe ArrayIndexDescriptors

analyzeExpression :: ExpressionAnalyzer op -> [PatElem (LetDec GPU)] -> op -> AnalyzeCtx -> StmtsAids
analyzeExpression f pp op ctx =
  M.fromList
    . zip (map patElemName pp)
    $ catMaybes -- remove all Nothings
    $ replicate (length pp)
    $ f op ctx -- create a Maybe result for each pattern

discardKeys :: StmtsAids -> ArrayIndexDescriptors
discardKeys =
  foldl (M.unionWith (++)) M.empty
    . map snd
    . M.toList

analyseStms :: [Stm GPU] -> AnalyzeCtx -> IterationType -> StmtsAids
analyseStms (stm : ss) ctx it =
  case stm of
    (Let (Pat pp) _ (BasicOp op)) ->
      let ctx' = extendCtx ctx pp (it, op)
       in let res = analyzeExpression analyseOp pp op ctx
           in M.union res $ analyseStms ss ctx' it
    -- Apply is still not matched, but is it relevant?
    (Let (Pat pp) _ (Match _subexps cases defaultBody _)) ->
      -- Just union the cases? no subexps?
      let bodies = (defaultBody : map caseBody cases)
       in let res' = foldl (M.unionWith (++)) M.empty $ map analyzeCase bodies
           in let res =
                    M.fromList
                      . zip (map patElemName pp)
                      . replicate (length pp) -- get vnames of patterns
                      $ res'
               in -- in let ctx' = extendCtx ctx pp (it, o)
                  M.union res $ analyseStms ss ctx it
      where
        analyzeCase body =
          let res = analyseStms (stmsToList $ bodyStms body) ctx it
           in foldl (M.unionWith (++)) M.empty
                . map snd
                $ M.toList res
    (Let (Pat _pp) _ (Loop bindings _ _body)) ->
      -- 0. Create temporary context
      let tCtx = M.fromList $ map (\(p, x) -> (paramName p, (Sequential, SubExp x))) bindings
       in -- 1. Run analysis on body with temporary context
          let res = analyzeExpression (\o c -> Just . discardKeys $ analyseStms o c Sequential) _pp (stmsToList $ bodyStms _body) tCtx
           in -- 2. recurse
              M.union res $ analyseStms ss ctx it
    (Let (Pat pp) _ (Op (SegOp op))) ->
      let res = analyzeExpression analyseKernelBody pp op ctx
       in M.union res $ analyseStms ss ctx it
    -- (Let (Pat pp) _ (Op (SegOp (SegRed _lvl _space _ops _type _kbody)))) ->
    -- SegScan & SegHist?
    --  undefined
    (Let (Pat pp) _ (Op (SizeOp op))) ->
      let ctx' = extendCtx ctx pp (it, sizeOpToCnst op)
       in analyseStms ss ctx' it
    (Let (Pat pp) _ (Op (GPUBody _ body))) ->
      -- TODO: Add to context ((somehow))
      -- We cant use analyze expression here :(
      let res' =
            foldl (M.unionWith (++)) M.empty
              . map snd
              . M.toList
              -- GPUBody is sequential!
              . (\p -> analyseStms p M.empty Sequential)
              . stmsToList
              $ bodyStms body
       in let res =
                M.fromList
                  . zip (map patElemName pp)
                  . replicate (length pp) -- get vnames of patterns
                  $ res'
           in -- in let ctx' = extendCtx ctx pp (it, o)
              M.union res $ analyseStms ss ctx it
    -- TODO: Add patterns here. ( which ones? )
    -- OtherOp (op rep)
    -- Match (op rep)
    (Let (Pat _pp) _ _op) ->
      analyseStms ss ctx it
analyseStms [] _ _ = M.empty

-- | Extend current `ctx` with segSpace defs, as parallel
analyseKernelBody :: SegOp SegLevel GPU -> AnalyzeCtx -> Maybe ArrayIndexDescriptors
analyseKernelBody op ctx =
  let ctx' = M.union ctx . M.fromList . map toCtx . unSegSpace $ segSpace op
   in Just $ analyseOpStm ctx' . stmsToList . kernelBodyStms $ segBody op
  where
    -- We extend the context by wrapping the operator in `SubExp`, effectively
    -- wrapping it into a BasicOp.
    -- We need to keep this in mind when checking context.
    toCtx (name, e) = (name, (Parallel, SubExp e))
    analyseOpStm :: AnalyzeCtx -> [Stm GPU] -> ArrayIndexDescriptors
    analyseOpStm ctx' prog =
      foldl (M.unionWith (++)) M.empty . toList $ analyseStms prog ctx' Parallel

-- | Construct MemoryAccessPattern using a `VName`
accesssPatternOfVName :: VName -> Variance -> MemoryAccessPattern
accesssPatternOfVName name =
  MemoryAccessPattern $ DimIndexExpression $ Left name

-- | Construct MemoryAccessPattern using a `PrimValue`
accesssPatternOfPrimValue :: PrimValue -> MemoryAccessPattern
accesssPatternOfPrimValue val =
  MemoryAccessPattern (DimIndexExpression $ Right val) Invariant

-- | Return the map of all access from a given BasicOperator
-- TODO: Implement other BasicOp patterns than Index
-- In reality, we should use the Maybe monad, instead of returning empty maps,
-- but i am lazy and we have a deadline.
analyseOp :: BasicOp -> AnalyzeCtx -> Maybe ArrayIndexDescriptors
analyseOp (Index name (Slice unslice)) ctx =
  Just $
    M.singleton name $
      L.singleton $
        map
          ( \case
              (DimFix (Constant primvalue)) ->
                accesssPatternOfPrimValue primvalue
              (DimFix (Var name')) ->
                case M.lookup name' ctx of
                  Nothing -> accesssPatternOfVName (VName "Missing" 42) (Variant Sequential)
                  Just (_, op) ->
                    case getOpVariance ctx op of
                      Nothing -> accesssPatternOfVName name' Invariant
                      Just var -> accesssPatternOfVName name' var
              (DimSlice {}) -> accesssPatternOfVName (VName "NicerSlicer" 42) (Variant Sequential)
          )
          unslice
analyseOp (BinOp {}) _ = Nothing
analyseOp _ _ = Nothing

getSubExpVariance :: AnalyzeCtx -> SubExp -> Maybe Variance
getSubExpVariance _ (Constant _) = Just Invariant
getSubExpVariance ctx (Var vname) =
  case M.lookup vname ctx of
    Nothing -> Just Invariant
    -- TODO: Is this the right way to do it?
    --  All let p = .. are marked as parallel inside kernel bodies
    --  The gtid is added to context with parallel, how do we distinguish them?
    Just (Parallel, _) -> Just $ Variant Parallel
    Just op -> getOpVariance ctx $ snd op

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
getOpVariance ctx (SubExp e) = getSubExpVariance ctx e
getOpVariance _ (ArrayLit (_e : _ee) _) = Nothing
getOpVariance ctx (UnOp _ e) = getSubExpVariance ctx e
getOpVariance ctx (BinOp _ l r) = getSubExpVariance ctx l >< getSubExpVariance ctx r
getOpVariance ctx (CmpOp _ l r) = getSubExpVariance ctx l >< getSubExpVariance ctx r
getOpVariance ctx (ConvOp _ e) = getSubExpVariance ctx e
getOpVariance ctx (Assert e _ _) = getSubExpVariance ctx e
-- Usually hit in indirect indices
getOpVariance ctx (Index _name (Slice ee)) = foldl (><) Nothing $ map (getSubExpVariance ctx . fromJust . dimFix) ee
--
getOpVariance ctx (Update _ _name (Slice dsts) _src) = getSubExpVariance ctx . fromJust . dimFix $ head dsts
getOpVariance ctx (FlatIndex _name (FlatSlice e _dims)) = getSubExpVariance ctx e
getOpVariance ctx (FlatUpdate _name1 (FlatSlice e _dims) _name2) = getSubExpVariance ctx e
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