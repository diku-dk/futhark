{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | High-level representation of SOACs.  When performing
-- SOAC-transformations, operating on normal 'Exp' values is somewhat
-- of a nuisance, as they can represent terms that are not proper
-- SOACs.  In contrast, this module exposes a SOAC representation that
-- does not enable invalid representations (except for type errors).
--
-- Furthermore, while standard normalised Futhark requires that the inputs
-- to a SOAC are variables or constants, the representation in this
-- module also supports various index-space transformations, like
-- @replicate@ or @rearrange@.  This is also very convenient when
-- implementing transformations.
--
-- The names exported by this module conflict with the standard Futhark
-- syntax tree constructors, so you are advised to use a qualified
-- import:
--
-- @
-- import Futhark.Analysis.HORep.SOAC (SOAC)
-- import qualified Futhark.Analysis.HORep.SOAC as SOAC
-- @
module Futhark.Analysis.HORep.SOAC
  ( -- * SOACs
    SOAC (..),
    Futhark.ScremaForm (..),
    inputs,
    setInputs,
    lambda,
    setLambda,
    typeOf,
    width,

    -- ** Converting to and from expressions
    NotSOAC (..),
    fromExp,
    toExp,
    toSOAC,

    -- * SOAC inputs
    Input (..),
    varInput,
    inputTransforms,
    identInput,
    isVarInput,
    isVarishInput,
    addTransform,
    addInitialTransforms,
    inputArray,
    inputRank,
    inputType,
    inputRowType,
    transformRows,
    transposeInput,
    applyTransforms,

    -- ** Input transformations
    ArrayTransforms,
    noTransforms,
    nullTransforms,
    (|>),
    (<|),
    viewf,
    ViewF (..),
    viewl,
    ViewL (..),
    ArrayTransform (..),
    transformFromExp,
    transformToExp,
    soacToStream,
  )
where

import Control.Monad (forM)
import Data.Foldable as Foldable
import Data.Maybe
import Data.Sequence qualified as Seq
import Futhark.Construct hiding (toExp)
import Futhark.IR hiding
  ( Index,
    Iota,
    Rearrange,
    Replicate,
    Reshape,
    typeOf,
  )
import Futhark.IR qualified as Futhark
import Futhark.IR.SOACS.SOAC
  ( HistOp (..),
    ScremaForm (..),
    scremaType,
  )
import Futhark.IR.SOACS.SOAC qualified as Futhark
import Futhark.Transform.Rename (renameLambda)
import Futhark.Transform.Substitute
import Futhark.Util.Pretty (pretty)
import Futhark.Util.Pretty qualified as PP

-- | A single, simple transformation.  If you want several, don't just
-- create a list, use 'ArrayTransforms' instead.
data ArrayTransform
  = -- | A permutation of an otherwise valid input.
    Rearrange (StmAux ()) [Int]
  | -- | A reshaping of an otherwise valid input.
    Reshape (StmAux ()) (NewShape SubExp)
  | -- | Replicate the rows of the array a number of times.
    Replicate (StmAux ()) Shape
  | -- | An array indexing operation.
    Index (StmAux ()) (Slice SubExp)
  deriving (Show, Eq, Ord)

instance FreeIn ArrayTransform where
  freeIn' (Rearrange cs _) = freeIn' cs
  freeIn' (Reshape cs shape) = freeIn' cs <> freeIn' shape
  freeIn' (Replicate cs shape) = freeIn' cs <> freeIn' shape
  freeIn' (Index cs slice) = freeIn' cs <> freeIn' slice

instance Substitute ArrayTransform where
  substituteNames substs (Rearrange cs xs) =
    Rearrange (substituteNames substs cs) xs
  substituteNames substs (Reshape cs newshape) =
    Reshape (substituteNames substs cs) (substituteNames substs newshape)
  substituteNames substs (Replicate cs se) =
    Replicate (substituteNames substs cs) (substituteNames substs se)
  substituteNames substs (Index cs slice) =
    Index (substituteNames substs cs) (substituteNames substs slice)

-- | A sequence of array transformations, heavily inspired by
-- "Data.Seq".  You can decompose it using 'viewf' and 'viewl', and
-- grow it by using '|>' and '<|'.  These correspond closely to the
-- similar operations for sequences, except that appending will try to
-- normalise and simplify the transformation sequence.
--
-- The data type is opaque in order to enforce normalisation
-- invariants.  Basically, when you grow the sequence, the
-- implementation will try to coalesce neighboring permutations, for
-- example by composing permutations and removing identity
-- transformations.
newtype ArrayTransforms = ArrayTransforms (Seq.Seq ArrayTransform)
  deriving (Eq, Ord, Show)

instance Semigroup ArrayTransforms where
  ts1 <> ts2 = case viewf ts2 of
    t :< ts2' -> (ts1 |> t) <> ts2'
    EmptyF -> ts1

instance Monoid ArrayTransforms where
  mempty = noTransforms

instance Substitute ArrayTransforms where
  substituteNames substs (ArrayTransforms ts) =
    ArrayTransforms $ substituteNames substs <$> ts

instance FreeIn ArrayTransforms where
  freeIn' (ArrayTransforms trs) = foldMap freeIn' trs

-- | The empty transformation list.
noTransforms :: ArrayTransforms
noTransforms = ArrayTransforms Seq.empty

-- | Is it an empty transformation list?
nullTransforms :: ArrayTransforms -> Bool
nullTransforms (ArrayTransforms s) = Seq.null s

-- | Decompose the input-end of the transformation sequence.
viewf :: ArrayTransforms -> ViewF
viewf (ArrayTransforms s) = case Seq.viewl s of
  t Seq.:< s' -> t :< ArrayTransforms s'
  Seq.EmptyL -> EmptyF

-- | A view of the first transformation to be applied.
data ViewF
  = EmptyF
  | ArrayTransform :< ArrayTransforms

-- | Decompose the output-end of the transformation sequence.
viewl :: ArrayTransforms -> ViewL
viewl (ArrayTransforms s) = case Seq.viewr s of
  s' Seq.:> t -> ArrayTransforms s' :> t
  Seq.EmptyR -> EmptyL

-- | A view of the last transformation to be applied.
data ViewL
  = EmptyL
  | ArrayTransforms :> ArrayTransform

-- | Add a transform to the end of the transformation list.
(|>) :: ArrayTransforms -> ArrayTransform -> ArrayTransforms
(|>) = flip $ addTransform' extract add $ uncurry (flip (,))
  where
    extract ts' = case viewl ts' of
      EmptyL -> Nothing
      ts'' :> t' -> Just (t', ts'')
    add t' (ArrayTransforms ts') = ArrayTransforms $ ts' Seq.|> t'

-- | Add a transform at the beginning of the transformation list.
(<|) :: ArrayTransform -> ArrayTransforms -> ArrayTransforms
(<|) = addTransform' extract add id
  where
    extract ts' = case viewf ts' of
      EmptyF -> Nothing
      t' :< ts'' -> Just (t', ts'')
    add t' (ArrayTransforms ts') = ArrayTransforms $ t' Seq.<| ts'

addTransform' ::
  (ArrayTransforms -> Maybe (ArrayTransform, ArrayTransforms)) ->
  (ArrayTransform -> ArrayTransforms -> ArrayTransforms) ->
  ((ArrayTransform, ArrayTransform) -> (ArrayTransform, ArrayTransform)) ->
  ArrayTransform ->
  ArrayTransforms ->
  ArrayTransforms
addTransform' extract add swap t ts =
  fromMaybe (t `add` ts) $ do
    (t', ts') <- extract ts
    combined <- uncurry combineTransforms $ swap (t', t)
    Just $
      if identityTransform combined
        then ts'
        else addTransform' extract add swap combined ts'

identityTransform :: ArrayTransform -> Bool
identityTransform (Rearrange _ perm) =
  Foldable.and $ zipWith (==) perm [0 ..]
identityTransform _ = False

combineTransforms :: ArrayTransform -> ArrayTransform -> Maybe ArrayTransform
combineTransforms (Rearrange cs2 perm2) (Rearrange cs1 perm1) =
  Just $ Rearrange (cs1 <> cs2) $ perm2 `rearrangeCompose` perm1
combineTransforms _ _ = Nothing

-- | Given an expression, determine whether the expression represents
-- an input transformation of an array variable.  If so, return the
-- variable and the transformation.  Only 'Rearrange' and 'Reshape'
-- are possible to express this way.
transformFromExp :: StmAux () -> Exp rep -> Maybe (VName, ArrayTransform)
transformFromExp cs (BasicOp (Futhark.Rearrange v perm)) =
  Just (v, Rearrange cs perm)
transformFromExp cs (BasicOp (Futhark.Reshape v shape)) =
  Just (v, Reshape cs shape)
transformFromExp cs (BasicOp (Futhark.Replicate shape (Var v))) =
  Just (v, Replicate cs shape)
transformFromExp cs (BasicOp (Futhark.Index v slice)) =
  Just (v, Index cs slice)
transformFromExp _ _ = Nothing

-- | Turn an array transform on an array back into an expression.
transformToExp :: (Monad m, HasScope rep m) => ArrayTransform -> VName -> m (StmAux (), Exp rep)
transformToExp (Replicate cs n) ia =
  pure (cs, BasicOp $ Futhark.Replicate n (Var ia))
transformToExp (Rearrange cs perm) ia = do
  r <- arrayRank <$> lookupType ia
  pure (cs, BasicOp $ Futhark.Rearrange ia (perm ++ [length perm .. r - 1]))
transformToExp (Reshape cs shape) ia = do
  pure (cs, BasicOp $ Futhark.Reshape ia shape)
transformToExp (Index cs slice) ia = do
  pure (cs, BasicOp $ Futhark.Index ia slice)

-- | One array input to a SOAC - a SOAC may have multiple inputs, but
-- all are of this form.  Only the array inputs are expressed with
-- this type; other arguments, such as initial accumulator values, are
-- plain expressions.  The transforms are done left-to-right, that is,
-- the first element of the 'ArrayTransform' list is applied first.
data Input = Input ArrayTransforms VName Type
  deriving (Show, Eq, Ord)

instance FreeIn Input where
  freeIn' (Input trs v t) = freeIn' trs <> freeIn' v <> freeIn' t

instance Substitute Input where
  substituteNames substs (Input ts v t) =
    Input
      (substituteNames substs ts)
      (substituteNames substs v)
      (substituteNames substs t)

-- | Create a plain array variable input with no transformations.
varInput :: (HasScope t f) => VName -> f Input
varInput v = withType <$> lookupType v
  where
    withType = Input (ArrayTransforms Seq.empty) v

-- | Create a plain array variable input with no transformations, from an 'Ident'.
identInput :: Ident -> Input
identInput v = Input (ArrayTransforms Seq.empty) (identName v) (identType v)

-- | If the given input is a plain variable input, with no transforms,
-- return the variable.
isVarInput :: Input -> Maybe VName
isVarInput (Input ts v _) | nullTransforms ts = Just v
isVarInput _ = Nothing

-- | If the given input is a plain variable input, with no non-vacuous
-- transforms, return the variable.
isVarishInput :: Input -> Maybe VName
isVarishInput (Input ts v t)
  | nullTransforms ts = Just v
  | Reshape aux newshape :< ts' <- viewf ts,
    ReshapeCoerce <- reshapeKind newshape,
    1 <- shapeRank $ newShape newshape,
    stmAuxCerts aux == mempty =
      isVarishInput $ Input ts' v t
isVarishInput _ = Nothing

-- | Add a transformation to the end of the transformation list.
addTransform :: ArrayTransform -> Input -> Input
addTransform tr (Input trs a t) =
  Input (trs |> tr) a t

-- | Add several transformations to the start of the transformation
-- list.
addInitialTransforms :: ArrayTransforms -> Input -> Input
addInitialTransforms ts (Input ots a t) = Input (ts <> ots) a t

applyTransform :: (MonadBuilder m) => ArrayTransform -> VName -> m VName
applyTransform tr ia = do
  (aux, e) <- transformToExp tr ia
  auxing aux $ letExp s e
  where
    s = case tr of
      Replicate {} -> "replicate"
      Rearrange {} -> "rearrange"
      Reshape {} -> "reshape"
      Index {} -> "index"

applyTransforms :: (MonadBuilder m) => ArrayTransforms -> VName -> m VName
applyTransforms (ArrayTransforms ts) a = foldlM (flip applyTransform) a ts

-- | Convert SOAC inputs to the corresponding expressions.
inputsToSubExps ::
  (MonadBuilder m) =>
  [Input] ->
  m [VName]
inputsToSubExps = mapM f
  where
    f (Input ts a _) = applyTransforms ts a

-- | Return the array name of the input.
inputArray :: Input -> VName
inputArray (Input _ v _) = v

-- | The transformations applied to an input.
inputTransforms :: Input -> ArrayTransforms
inputTransforms (Input ts _ _) = ts

-- | Return the type of an input.
inputType :: Input -> Type
inputType (Input (ArrayTransforms ts) _ at) =
  Foldable.foldl transformType at ts
  where
    transformType t (Replicate _ shape) =
      arrayOfShape t shape
    transformType t (Rearrange _ perm) =
      rearrangeType perm t
    transformType t (Reshape _ shape) =
      t `setArrayShape` newShape shape
    transformType t (Index _ slice) =
      t `setArrayShape` sliceShape slice

-- | Return the row type of an input.  Just a convenient alias.
inputRowType :: Input -> Type
inputRowType = rowType . inputType

-- | Return the array rank (dimensionality) of an input.  Just a
-- convenient alias.
inputRank :: Input -> Int
inputRank = arrayRank . inputType

-- | Apply the transformations to every row of the input.
transformRows :: ArrayTransforms -> Input -> Input
transformRows (ArrayTransforms ts) =
  flip (Foldable.foldl transformRows') ts
  where
    transformRows' inp (Rearrange cs perm) =
      addTransform (Rearrange cs (0 : map (+ 1) perm)) inp
    transformRows' inp (Reshape cs shape) =
      addTransform (Reshape cs newshape) inp
      where
        newshape = reshapeAll inp_shape $ Shape [shapeSize 0 inp_shape] <> newShape shape
        inp_shape = arrayShape $ inputType inp
    transformRows' inp (Replicate cs n)
      | inputRank inp == 1 =
          Rearrange mempty [1, 0]
            `addTransform` (Replicate cs n `addTransform` inp)
      | otherwise =
          Rearrange mempty (2 : 0 : 1 : [3 .. inputRank inp])
            `addTransform` ( Replicate cs n
                               `addTransform` (Rearrange mempty (1 : 0 : [2 .. inputRank inp - 1]) `addTransform` inp)
                           )
    transformRows' inp nts =
      error $ "transformRows: Cannot transform this yet:\n" ++ show nts ++ "\n" ++ show inp

-- | Add to the input a 'Rearrange' transform that performs an @(k,n)@
-- transposition.  The new transform will be at the end of the current
-- transformation list.
transposeInput :: Int -> Int -> Input -> Input
transposeInput k n inp =
  addTransform (Rearrange mempty $ transposeIndex k n [0 .. inputRank inp - 1]) inp

-- | A definite representation of a SOAC expression.
data SOAC rep
  = Stream SubExp [Input] [SubExp] (Lambda rep)
  | Screma SubExp [Input] (ScremaForm rep)
  | Hist SubExp [Input] [HistOp rep] (Lambda rep)
  deriving (Eq, Show)

-- | Returns the inputs used in a SOAC.
inputs :: SOAC rep -> [Input]
inputs (Stream _ arrs _ _) = arrs
inputs (Screma _ arrs _) = arrs
inputs (Hist _ inps _ _) = inps

-- | Set the inputs to a SOAC.
setInputs :: [Input] -> SOAC rep -> SOAC rep
setInputs arrs (Stream w _ nes lam) =
  Stream (newWidth arrs w) arrs nes lam
setInputs arrs (Screma w _ form) =
  Screma w arrs form
setInputs inps (Hist w _ ops lam) =
  Hist w inps ops lam

newWidth :: [Input] -> SubExp -> SubExp
newWidth [] w = w
newWidth (inp : _) _ = arraySize 0 $ inputType inp

-- | The lambda used in a given SOAC.
lambda :: SOAC rep -> Lambda rep
lambda (Stream _ _ _ lam) = lam
lambda (Screma _ _ (ScremaForm lam _ _ _)) = lam
lambda (Hist _ _ _ lam) = lam

-- | Set the lambda used in the SOAC.
setLambda :: Lambda rep -> SOAC rep -> SOAC rep
setLambda lam (Stream w arrs nes _) =
  Stream w arrs nes lam
setLambda lam (Screma w arrs (ScremaForm _ scan red post_lam)) =
  Screma w arrs (ScremaForm lam scan red post_lam)
setLambda lam (Hist w ops inps _) =
  Hist w ops inps lam

-- | The return type of a SOAC.
typeOf :: SOAC rep -> [Type]
typeOf (Stream w _ nes lam) =
  let accrtps = take (length nes) $ lambdaReturnType lam
      arrtps =
        [ arrayOf (stripArray 1 t) (Shape [w]) NoUniqueness
        | t <- drop (length nes) (lambdaReturnType lam)
        ]
   in accrtps ++ arrtps
typeOf (Screma w _ form) =
  scremaType w form
typeOf (Hist _ _ ops _) = do
  op <- ops
  map (`arrayOfShape` histShape op) (lambdaReturnType $ histOp op)

-- | The "width" of a SOAC is the expected outer size of its array
-- inputs _after_ input-transforms have been carried out.
width :: SOAC rep -> SubExp
width (Stream w _ _ _) = w
width (Screma w _ _) = w
width (Hist w _ _ _) = w

-- | Convert a SOAC to the corresponding expression.
toExp ::
  (MonadBuilder m, Op (Rep m) ~ Futhark.SOAC (Rep m)) =>
  SOAC (Rep m) ->
  m (Exp (Rep m))
toExp soac = Op <$> toSOAC soac

-- | Convert a SOAC to a Futhark-level SOAC.
toSOAC :: (MonadBuilder m) => SOAC (Rep m) -> m (Futhark.SOAC (Rep m))
toSOAC (Stream w inps nes lam) =
  Futhark.Stream w <$> inputsToSubExps inps <*> pure nes <*> pure lam
toSOAC (Screma w arrs form) =
  Futhark.Screma w <$> inputsToSubExps arrs <*> pure form
toSOAC (Hist w arrs ops lam) =
  Futhark.Hist w <$> inputsToSubExps arrs <*> pure ops <*> pure lam

-- | The reason why some expression cannot be converted to a 'SOAC'
-- value.
data NotSOAC
  = -- | The expression is not a (tuple-)SOAC at all.
    NotSOAC
  deriving (Show)

-- | Either convert an expression to the normalised SOAC
-- representation, or a reason why the expression does not have the
-- valid form.
fromExp ::
  (Op rep ~ Futhark.SOAC rep, HasScope rep m) =>
  Exp rep ->
  m (Either NotSOAC (SOAC rep))
fromExp (Op (Futhark.Stream w as nes lam)) =
  Right <$> (Stream w <$> traverse varInput as <*> pure nes <*> pure lam)
fromExp (Op (Futhark.Screma w arrs form)) =
  Right <$> (Screma w <$> traverse varInput arrs <*> pure form)
fromExp (Op (Futhark.Hist w arrs ops lam)) =
  Right <$> (Hist w <$> traverse varInput arrs <*> pure ops <*> pure lam)
fromExp _ = pure $ Left NotSOAC

-- | To-Stream translation of SOACs.
--   Returns the Stream SOAC and the
--   extra-accumulator body-result ident if any.
soacToStream ::
  ( HasScope rep m,
    MonadFreshNames m,
    Buildable rep,
    BuilderOps rep,
    Op rep ~ Futhark.SOAC rep
  ) =>
  SOAC rep ->
  m (SOAC rep, [Ident], Stms rep)
soacToStream soac = do
  chunk_param <- newParam "chunk" $ Prim int64
  let chvar = Var $ paramName chunk_param
      (lam, inps) = (lambda soac, inputs soac)
      w = width soac
  lam' <- renameLambda lam
  let arrrtps = mapType w lam
      -- the chunked-outersize of the array result and input types
      loutps = [arrayOfRow t chvar | t <- map rowType arrrtps]
      lintps = [arrayOfRow t chvar | t <- map inputRowType inps]

  strm_inpids <- mapM (newParam "inp") lintps
  -- Treat each SOAC case individually:
  case soac of
    Screma _ _ form
      | Just _ <- Futhark.isMapSOAC form -> do
          -- Map(f,a) => is translated in strem's body to:
          -- let strm_resids = map(f,a_ch) in strm_resids
          --
          -- array result and input IDs of the stream's lambda
          strm_resids <- mapM (newIdent "res") loutps
          insoac <-
            Futhark.Screma chvar (map paramName strm_inpids)
              <$> Futhark.mapSOAC lam'
          let insstm = mkLet strm_resids $ Op insoac
              strmbdy = mkBody (oneStm insstm) $ map (subExpRes . Var . identName) strm_resids
              strmpar = chunk_param : strm_inpids
              strmlam = Lambda strmpar loutps strmbdy
          -- map(f,a) creates a stream with NO accumulators
          pure (Stream w inps [] strmlam, [], mempty)
      | Just (scans, _) <- Futhark.isScanomapSOAC form -> do
          -- Scanomap(scan_lam, map_lam, a) is converted to a stream
          -- whose body is itself a (neutral-free) scanomap Screma SOAC
          -- over the chunk, plus a top-level shift map that combines
          -- each per-chunk scan output with the cross-chunk
          -- accumulator inherited from the previous chunk.  Keeping
          -- the per-chunk scan as a SOAC (rather than a hand-written
          -- for-loop) is important because
          -- `sequentialStreamWholeArray` later inlines the body with
          -- chunk = w, and `ExtractMulticore` /`ExtractKernels` then
          -- recognise the inlined Screma as a parallel scanomap and
          -- extract it as a `SegScan`.
          --
          -- Without a neutral element, the very first chunk has no
          -- valid `acc` to shift by, so we guard the shift with the
          -- usual `is_first` toggle and similarly guard the
          -- cross-chunk accumulator update.  In the common case
          -- `sequentialStreamWholeArray` makes `is_first` the
          -- constant `true`, so the `if` collapses to the then-branch
          -- and the shift map disappears, leaving a single full-width
          -- scanomap.
          let Futhark.Scan scan_lam = Futhark.singleScan scans
              num_scan_res = length $ lambdaReturnType scan_lam
              scan_ts = take num_scan_res $ lambdaReturnType lam

          is_first_param <- newParam "is_first" $ Prim Bool
          inpacc_ids <- mapM (newParam "inpacc") scan_ts

          strmlam <-
            fmap fst . runBuilder
              . mkLambda
                (chunk_param : is_first_param : inpacc_ids ++ strm_inpids)
              $ do
                -- 1. Per-chunk neutral-free scanomap (a Screma SOAC).
                scan_form <-
                  Futhark.scanomapSOAC [Futhark.Scan scan_lam] lam'
                chunk_res <-
                  letTupExp "chunk_scan" . Op $
                    Futhark.Screma chvar (map paramName strm_inpids) scan_form
                let (scan_chunk_vs, map_chunk_vs) =
                      splitAt num_scan_res chunk_res

                -- 2. Shift map: combine each scan_chunk[j] with the
                -- cross-chunk acc using scan_lam.  On the first
                -- chunk we skip the shift (no valid acc).
                shift_lam <-
                  mkScanShiftLam scan_lam (map paramName inpacc_ids)
                shifted_scan_vs <-
                  letTupExp "scan_chunk_shifted"
                    =<< eIf
                      (eSubExp . Var $ paramName is_first_param)
                      (resultBodyM $ map Var scan_chunk_vs)
                      ( buildBody_ $ do
                          shifted_v <-
                            letTupExp "shifted" . Op
                              =<< (Futhark.Screma chvar scan_chunk_vs <$> Futhark.mapSOAC shift_lam)
                          pure $ map (subExpRes . Var) shifted_v
                      )

                -- 3. New cross-chunk acc.  Take the last element of
                -- each per-chunk scan and combine with the inherited
                -- acc (or use it directly on the first chunk).
                cm1 <-
                  letSubExp "cm1" . Futhark.BasicOp $
                    Futhark.BinOp
                      (Sub Int64 OverflowWrap)
                      chvar
                      (Futhark.intConst Int64 1)
                last_vs <-
                  forM (zip scan_chunk_vs scan_ts) $ \(arr, t) ->
                    letSubExp "last_v" . Futhark.BasicOp $
                      Futhark.Index
                        arr
                        (fullSlice (arrayOfRow t chvar) [DimFix cm1])
                new_acc_vs <-
                  letTupExp "new_acc"
                    =<< eIf
                      (eSubExp . Var $ paramName is_first_param)
                      (resultBodyM last_vs)
                      ( buildBody_ $ do
                          combine_lam <- renameLambda scan_lam
                          let (xparams, yparams) =
                                splitAt num_scan_res $ lambdaParams combine_lam
                          forM_ (zip xparams (map (Var . paramName) inpacc_ids)) $
                            \(p, v) ->
                              letBindNames [paramName p]
                                . Futhark.BasicOp
                                . Futhark.SubExp
                                $ v
                          forM_ (zip yparams last_vs) $ \(p, v) ->
                            letBindNames [paramName p]
                              . Futhark.BasicOp
                              . Futhark.SubExp
                              $ v
                          addStms . bodyStms $ lambdaBody combine_lam
                          pure . bodyResult $ lambdaBody combine_lam
                      )

                -- Return (False, new_acc..., shifted_scan_chunk..., map_chunk...)
                pure $
                  subExpRes (Futhark.Constant $ BoolValue False)
                    : map (subExpRes . Var) new_acc_vs
                    ++ map (subExpRes . Var) shifted_scan_vs
                    ++ map (subExpRes . Var) map_chunk_vs

          -- Initial cross-chunk accumulators: is_first=True, scan
          -- accs = blank (never read because of the is_first guard).
          let blankScanAcc (Prim pt) = Futhark.Constant $ blankPrimValue pt
              blankScanAcc t = error $ "soacToStream: non-primitive scan acc type: " <> show t
              blank_accs = map blankScanAcc scan_ts
              init_ne = Futhark.Constant (BoolValue True) : blank_accs
              newacc_idents =
                Ident (paramName is_first_param) (Prim Bool)
                  : zipWith (\p t -> Ident (paramName p) t) inpacc_ids scan_ts
          pure (Stream w inps init_ne strmlam, newacc_idents, mempty)
      | Just (reds, _) <- Futhark.isRedomapSOAC form,
        Futhark.Reduce comm lamin nes <- Futhark.singleReduce reds -> do
          -- Redomap(+,lam,nes,a) => is translated in strem's body to:
          -- 1. let (acc0_ids,strm_resids) = redomap(+,lam,nes,a_ch) in
          -- 2. let acc'                   = acc + acc0_ids          in
          --    {acc', strm_resids}

          let accrtps = take (length nes) $ lambdaReturnType lam
              -- the chunked-outersize of the array result and input types
              loutps' = drop (length nes) loutps
              -- the lambda with proper index
              foldlam = lam'
          -- array result and input IDs of the stream's lambda
          strm_resids <- mapM (newIdent "res") loutps'
          inpacc_ids <- mapM (newParam "inpacc") accrtps
          acc0_ids <- mapM (newIdent "acc0") accrtps
          -- 1. let (acc0_ids,strm_resids) = redomap(+,lam,nes,a_ch) in
          insoac <-
            Futhark.Screma chvar (map paramName strm_inpids)
              <$> Futhark.redomapSOAC [Futhark.Reduce comm lamin nes] foldlam
          let insstm = mkLet (acc0_ids ++ strm_resids) $ Op insoac
          -- 2. let acc'     = acc + acc0_ids    in
          addaccbdy <-
            mkPlusBnds lamin . map Var $
              map paramName inpacc_ids ++ map identName acc0_ids
          -- Construct the stream
          let (addaccstm, addaccres) = (bodyStms addaccbdy, bodyResult addaccbdy)
              strmbdy =
                mkBody (oneStm insstm <> addaccstm) $
                  addaccres ++ map (subExpRes . Var . identName) strm_resids
              strmpar = chunk_param : inpacc_ids ++ strm_inpids
              strmlam = Lambda strmpar (accrtps ++ loutps') strmbdy
          pure (Stream w inps nes strmlam, [], mempty)

    -- Otherwise it cannot become a stream.
    _ -> pure (soac, [], mempty)
  where
    mkPlusBnds ::
      (MonadFreshNames m, Buildable rep) =>
      Lambda rep ->
      [SubExp] ->
      m (Body rep)
    mkPlusBnds plus accels = do
      plus' <- renameLambda plus
      let parstms =
            zipWith
              (\par se -> mkLet [paramIdent par] (BasicOp $ SubExp se))
              (lambdaParams plus')
              accels
          body = lambdaBody plus'
      pure $ body {bodyStms = stmsFromList parstms <> bodyStms body}

    -- Build a unary lambda over the scan element types that
    -- applies scan_lam with the inpacc names bound to its x-side
    -- parameters, returning `scan_lam(inpacc, y)`.  Used to shift
    -- a per-chunk scan output by the cross-chunk accumulator.
    mkScanShiftLam ::
      (MonadFreshNames m, Buildable rep) =>
      Lambda rep ->
      [VName] ->
      m (Lambda rep)
    mkScanShiftLam scan_lam inpaccs = do
      scan_lam' <- renameLambda scan_lam
      let n = length inpaccs
          (xparams, yparams) = splitAt n (lambdaParams scan_lam')
          parstms =
            zipWith
              ( \par v ->
                  mkLet [paramIdent par] . BasicOp . SubExp $ Var v
              )
              xparams
              inpaccs
          new_body =
            (lambdaBody scan_lam')
              { bodyStms =
                  stmsFromList parstms <> bodyStms (lambdaBody scan_lam')
              }
      pure $ Lambda yparams (lambdaReturnType scan_lam') new_body

ppArrayTransform :: PP.Doc a -> ArrayTransform -> PP.Doc a
ppArrayTransform e (Rearrange cs perm) =
  "rearrange" <> pretty cs <> PP.apply [PP.apply (map pretty perm), e]
ppArrayTransform e (Reshape cs shape) =
  "reshape" <> pretty cs <> PP.apply [pretty shape, e]
ppArrayTransform e (Replicate cs ne) =
  "replicate" <> pretty cs <> PP.apply [pretty ne, e]
ppArrayTransform e (Index cs slice) =
  e <> pretty cs <> pretty slice

instance PP.Pretty Input where
  pretty (Input (ArrayTransforms ts) arr _) = foldl ppArrayTransform (pretty arr) ts

instance PP.Pretty ArrayTransform where
  pretty = ppArrayTransform "INPUT"

instance (PrettyRep rep) => PP.Pretty (SOAC rep) where
  pretty (Screma w arrs form) = Futhark.ppScrema w arrs form
  pretty (Hist len imgs ops bucket_fun) = Futhark.ppHist len imgs ops bucket_fun
  pretty (Stream w arrs nes lam) = Futhark.ppStream w arrs nes lam
