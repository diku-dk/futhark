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

import Data.Foldable as Foldable
import Data.Maybe
import Data.Sequence qualified as Seq
import Futhark.Construct hiding (toExp)
import Futhark.IR hiding
  ( Iota,
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
    Rearrange Certs [Int]
  | -- | A reshaping of an otherwise valid input.
    Reshape Certs ReshapeKind Shape
  | -- | A reshaping of the outer dimension.
    ReshapeOuter Certs ReshapeKind Shape
  | -- | A reshaping of everything but the outer dimension.
    ReshapeInner Certs ReshapeKind Shape
  | -- | Replicate the rows of the array a number of times.
    Replicate Certs Shape
  deriving (Show, Eq, Ord)

instance Substitute ArrayTransform where
  substituteNames substs (Rearrange cs xs) =
    Rearrange (substituteNames substs cs) xs
  substituteNames substs (Reshape cs k ses) =
    Reshape (substituteNames substs cs) k (substituteNames substs ses)
  substituteNames substs (ReshapeOuter cs k ses) =
    ReshapeOuter (substituteNames substs cs) k (substituteNames substs ses)
  substituteNames substs (ReshapeInner cs k ses) =
    ReshapeInner (substituteNames substs cs) k (substituteNames substs ses)
  substituteNames substs (Replicate cs se) =
    Replicate (substituteNames substs cs) (substituteNames substs se)

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
transformFromExp :: Certs -> Exp rep -> Maybe (VName, ArrayTransform)
transformFromExp cs (BasicOp (Futhark.Rearrange perm v)) =
  Just (v, Rearrange cs perm)
transformFromExp cs (BasicOp (Futhark.Reshape k shape v)) =
  Just (v, Reshape cs k shape)
transformFromExp cs (BasicOp (Futhark.Replicate shape (Var v))) =
  Just (v, Replicate cs shape)
transformFromExp _ _ = Nothing

-- | Turn an array transform on an array back into an expression.
transformToExp :: (Monad m, HasScope rep m) => ArrayTransform -> VName -> m (Certs, Exp rep)
transformToExp (Replicate cs n) ia =
  pure (cs, BasicOp $ Futhark.Replicate n (Var ia))
transformToExp (Rearrange cs perm) ia = do
  r <- arrayRank <$> lookupType ia
  pure (cs, BasicOp $ Futhark.Rearrange (perm ++ [length perm .. r - 1]) ia)
transformToExp (Reshape cs k shape) ia = do
  pure (cs, BasicOp $ Futhark.Reshape k shape ia)
transformToExp (ReshapeOuter cs k shape) ia = do
  shape' <- reshapeOuter shape 1 . arrayShape <$> lookupType ia
  pure (cs, BasicOp $ Futhark.Reshape k shape' ia)
transformToExp (ReshapeInner cs k shape) ia = do
  shape' <- reshapeInner shape 1 . arrayShape <$> lookupType ia
  pure (cs, BasicOp $ Futhark.Reshape k shape' ia)

-- | One array input to a SOAC - a SOAC may have multiple inputs, but
-- all are of this form.  Only the array inputs are expressed with
-- this type; other arguments, such as initial accumulator values, are
-- plain expressions.  The transforms are done left-to-right, that is,
-- the first element of the 'ArrayTransform' list is applied first.
data Input = Input ArrayTransforms VName Type
  deriving (Show, Eq, Ord)

instance Substitute Input where
  substituteNames substs (Input ts v t) =
    Input
      (substituteNames substs ts)
      (substituteNames substs v)
      (substituteNames substs t)

-- | Create a plain array variable input with no transformations.
varInput :: HasScope t f => VName -> f Input
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
  | Reshape cs ReshapeCoerce (Shape [_]) :< ts' <- viewf ts,
    cs == mempty =
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

applyTransform :: MonadBuilder m => ArrayTransform -> VName -> m VName
applyTransform tr ia = do
  (cs, e) <- transformToExp tr ia
  certifying cs $ letExp s e
  where
    s = case tr of
      Replicate {} -> "replicate"
      Rearrange {} -> "rearrange"
      Reshape {} -> "reshape"
      ReshapeOuter {} -> "reshape_outer"
      ReshapeInner {} -> "reshape_inner"

applyTransforms :: MonadBuilder m => ArrayTransforms -> VName -> m VName
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
    transformType t (Reshape _ _ shape) =
      t `setArrayShape` shape
    transformType t (ReshapeOuter _ _ shape) =
      let Shape oldshape = arrayShape t
       in t `setArrayShape` Shape (shapeDims shape ++ drop 1 oldshape)
    transformType t (ReshapeInner _ _ shape) =
      let Shape oldshape = arrayShape t
       in t `setArrayShape` Shape (take 1 oldshape ++ shapeDims shape)

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
    transformRows' inp (Reshape cs k shape) =
      addTransform (ReshapeInner cs k shape) inp
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
  = Stream SubExp (Lambda rep) [SubExp] [Input]
  | Scatter SubExp (Lambda rep) [Input] [(Shape, Int, VName)]
  | Screma SubExp (ScremaForm rep) [Input]
  | Hist SubExp [HistOp rep] (Lambda rep) [Input]
  deriving (Eq, Show)

instance PP.Pretty Input where
  pretty (Input (ArrayTransforms ts) arr _) = foldl f (pretty arr) ts
    where
      f e (Rearrange cs perm) =
        "rearrange" <> pretty cs <> PP.apply [PP.apply (map pretty perm), e]
      f e (Reshape cs ReshapeArbitrary shape) =
        "reshape" <> pretty cs <> PP.apply [pretty shape, e]
      f e (ReshapeOuter cs ReshapeArbitrary shape) =
        "reshape_outer" <> pretty cs <> PP.apply [pretty shape, e]
      f e (ReshapeInner cs ReshapeArbitrary shape) =
        "reshape_inner" <> pretty cs <> PP.apply [pretty shape, e]
      f e (Reshape cs ReshapeCoerce shape) =
        "coerce" <> pretty cs <> PP.apply [pretty shape, e]
      f e (ReshapeOuter cs ReshapeCoerce shape) =
        "coerce_outer" <> pretty cs <> PP.apply [pretty shape, e]
      f e (ReshapeInner cs ReshapeCoerce shape) =
        "coerce_inner" <> pretty cs <> PP.apply [pretty shape, e]
      f e (Replicate cs ne) =
        "replicate" <> pretty cs <> PP.apply [pretty ne, e]

instance PrettyRep rep => PP.Pretty (SOAC rep) where
  pretty (Screma w form arrs) = Futhark.ppScrema w arrs form
  pretty (Hist len ops bucket_fun imgs) = Futhark.ppHist len imgs ops bucket_fun
  pretty (Stream w lam nes arrs) = Futhark.ppStream w arrs nes lam
  pretty (Scatter w lam arrs dests) = Futhark.ppScatter w arrs lam dests

-- | Returns the inputs used in a SOAC.
inputs :: SOAC rep -> [Input]
inputs (Stream _ _ _ arrs) = arrs
inputs (Scatter _len _lam ivs _as) = ivs
inputs (Screma _ _ arrs) = arrs
inputs (Hist _ _ _ inps) = inps

-- | Set the inputs to a SOAC.
setInputs :: [Input] -> SOAC rep -> SOAC rep
setInputs arrs (Stream w lam nes _) =
  Stream (newWidth arrs w) lam nes arrs
setInputs arrs (Scatter w lam _ivs as) =
  Scatter (newWidth arrs w) lam arrs as
setInputs arrs (Screma w form _) =
  Screma w form arrs
setInputs inps (Hist w ops lam _) =
  Hist w ops lam inps

newWidth :: [Input] -> SubExp -> SubExp
newWidth [] w = w
newWidth (inp : _) _ = arraySize 0 $ inputType inp

-- | The lambda used in a given SOAC.
lambda :: SOAC rep -> Lambda rep
lambda (Stream _ lam _ _) = lam
lambda (Scatter _len lam _ivs _as) = lam
lambda (Screma _ (ScremaForm _ _ lam) _) = lam
lambda (Hist _ _ lam _) = lam

-- | Set the lambda used in the SOAC.
setLambda :: Lambda rep -> SOAC rep -> SOAC rep
setLambda lam (Stream w _ nes arrs) =
  Stream w lam nes arrs
setLambda lam (Scatter len _lam ivs as) =
  Scatter len lam ivs as
setLambda lam (Screma w (ScremaForm scan red _) arrs) =
  Screma w (ScremaForm scan red lam) arrs
setLambda lam (Hist w ops _ inps) =
  Hist w ops lam inps

-- | The return type of a SOAC.
typeOf :: SOAC rep -> [Type]
typeOf (Stream w lam nes _) =
  let accrtps = take (length nes) $ lambdaReturnType lam
      arrtps =
        [ arrayOf (stripArray 1 t) (Shape [w]) NoUniqueness
          | t <- drop (length nes) (lambdaReturnType lam)
        ]
   in accrtps ++ arrtps
typeOf (Scatter _w lam _ivs dests) =
  zipWith arrayOfShape val_ts ws
  where
    indexes = sum $ zipWith (*) ns $ map length ws
    val_ts = drop indexes $ lambdaReturnType lam
    (ws, ns, _) = unzip3 dests
typeOf (Screma w form _) =
  scremaType w form
typeOf (Hist _ ops _ _) = do
  op <- ops
  map (`arrayOfShape` histShape op) (lambdaReturnType $ histOp op)

-- | The "width" of a SOAC is the expected outer size of its array
-- inputs _after_ input-transforms have been carried out.
width :: SOAC rep -> SubExp
width (Stream w _ _ _) = w
width (Scatter len _lam _ivs _as) = len
width (Screma w _ _) = w
width (Hist w _ _ _) = w

-- | Convert a SOAC to the corresponding expression.
toExp ::
  (MonadBuilder m, Op (Rep m) ~ Futhark.SOAC (Rep m)) =>
  SOAC (Rep m) ->
  m (Exp (Rep m))
toExp soac = Op <$> toSOAC soac

-- | Convert a SOAC to a Futhark-level SOAC.
toSOAC :: MonadBuilder m => SOAC (Rep m) -> m (Futhark.SOAC (Rep m))
toSOAC (Stream w lam nes inps) =
  Futhark.Stream w <$> inputsToSubExps inps <*> pure nes <*> pure lam
toSOAC (Scatter w lam ivs dests) =
  Futhark.Scatter w <$> inputsToSubExps ivs <*> pure lam <*> pure dests
toSOAC (Screma w form arrs) =
  Futhark.Screma w <$> inputsToSubExps arrs <*> pure form
toSOAC (Hist w ops lam arrs) =
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
  Right . Stream w lam nes <$> traverse varInput as
fromExp (Op (Futhark.Scatter w ivs lam as)) =
  Right <$> (Scatter w lam <$> traverse varInput ivs <*> pure as)
fromExp (Op (Futhark.Screma w arrs form)) =
  Right . Screma w form <$> traverse varInput arrs
fromExp (Op (Futhark.Hist w arrs ops lam)) =
  Right . Hist w ops lam <$> traverse varInput arrs
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
  m (SOAC rep, [Ident])
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
    Screma _ form _
      | Just _ <- Futhark.isMapSOAC form -> do
          -- Map(f,a) => is translated in strem's body to:
          -- let strm_resids = map(f,a_ch) in strm_resids
          --
          -- array result and input IDs of the stream's lambda
          strm_resids <- mapM (newIdent "res") loutps
          let insoac =
                Futhark.Screma chvar (map paramName strm_inpids) $
                  Futhark.mapSOAC lam'
              insstm = mkLet strm_resids $ Op insoac
              strmbdy = mkBody (oneStm insstm) $ map (subExpRes . Var . identName) strm_resids
              strmpar = chunk_param : strm_inpids
              strmlam = Lambda strmpar strmbdy loutps
          -- map(f,a) creates a stream with NO accumulators
          pure (Stream w strmlam [] inps, [])
      | Just (scans, _) <- Futhark.isScanomapSOAC form,
        Futhark.Scan scan_lam nes <- Futhark.singleScan scans -> do
          -- scanomap(scan_lam,nes,map_lam,a) => is translated in strem's body to:
          -- 1. let (scan0_ids,map_resids)   = scanomap(scan_lam, nes, map_lam, a_ch)
          -- 2. let strm_resids = map (acc `+`,nes, scan0_ids)
          -- 3. let outerszm1id = sizeof(0,strm_resids) - 1
          -- 4. let lasteel_ids = if outerszm1id < 0
          --                      then nes
          --                      else strm_resids[outerszm1id]
          -- 5. let acc'        = acc + lasteel_ids
          --    {acc', strm_resids, map_resids}
          -- the array and accumulator result types
          let scan_arr_ts = map (`arrayOfRow` chvar) $ lambdaReturnType scan_lam
              accrtps = lambdaReturnType scan_lam

          inpacc_ids <- mapM (newParam "inpacc") accrtps
          maplam <- mkMapPlusAccLam (map (Var . paramName) inpacc_ids) scan_lam
          -- Finally, construct the stream
          let strmpar = chunk_param : inpacc_ids ++ strm_inpids
          strmlam <- fmap fst . runBuilder . mkLambda strmpar $ do
            -- 1. let (scan0_ids,map_resids)  = scanomap(scan_lam,nes,map_lam,a_ch)
            (scan0_ids, map_resids) <-
              fmap (splitAt (length scan_arr_ts)) . letTupExp "scan" . Op $
                Futhark.Screma chvar (map paramName strm_inpids) $
                  Futhark.scanomapSOAC [Futhark.Scan scan_lam nes] lam'
            -- 2. let outerszm1id = chunksize - 1
            outszm1id <-
              letSubExp "outszm1" . BasicOp $
                BinOp
                  (Sub Int64 OverflowUndef)
                  (Var $ paramName chunk_param)
                  (constant (1 :: Int64))
            empty_arr <-
              letExp "empty_arr" . BasicOp $
                CmpOp
                  (CmpSlt Int64)
                  outszm1id
                  (constant (0 :: Int64))
            -- 3. let lasteel_ids = ...
            let indexLast arr = do
                  arr_t <- lookupType arr
                  pure $ BasicOp . Index arr $ fullSlice arr_t [DimFix outszm1id]
            lastel_ids <-
              letTupExp "lastel"
                =<< eIf
                  (eSubExp $ Var empty_arr)
                  (resultBodyM nes)
                  (eBody $ map indexLast scan0_ids)
            addlelbdy <-
              mkPlusBnds scan_lam $ map Var $ map paramName inpacc_ids ++ lastel_ids
            let (addlelstm, addlelres) = (bodyStms addlelbdy, bodyResult addlelbdy)
            -- 4. let strm_resids = map (acc `+`,nes, scan0_ids)
            strm_resids <-
              letTupExp "strm_res" . Op $
                Futhark.Screma chvar scan0_ids (Futhark.mapSOAC maplam)
            -- 5. let acc'        = acc + lasteel_ids
            addStms addlelstm
            pure $ addlelres ++ map (subExpRes . Var) (strm_resids ++ map_resids)
          pure
            ( Stream w strmlam nes inps,
              map paramIdent inpacc_ids
            )
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
          let insoac =
                Futhark.Screma
                  chvar
                  (map paramName strm_inpids)
                  $ Futhark.redomapSOAC [Futhark.Reduce comm lamin nes] foldlam
              insstm = mkLet (acc0_ids ++ strm_resids) $ Op insoac
          -- 2. let acc'     = acc + acc0_ids    in
          addaccbdy <-
            mkPlusBnds lamin $
              map Var $
                map paramName inpacc_ids ++ map identName acc0_ids
          -- Construct the stream
          let (addaccstm, addaccres) = (bodyStms addaccbdy, bodyResult addaccbdy)
              strmbdy =
                mkBody (oneStm insstm <> addaccstm) $
                  addaccres ++ map (subExpRes . Var . identName) strm_resids
              strmpar = chunk_param : inpacc_ids ++ strm_inpids
              strmlam = Lambda strmpar strmbdy (accrtps ++ loutps')
          pure (Stream w strmlam nes inps, [])

    -- Otherwise it cannot become a stream.
    _ -> pure (soac, [])
  where
    mkMapPlusAccLam ::
      (MonadFreshNames m, Buildable rep) =>
      [SubExp] ->
      Lambda rep ->
      m (Lambda rep)
    mkMapPlusAccLam accs plus = do
      let (accpars, rempars) = splitAt (length accs) $ lambdaParams plus
          parstms =
            zipWith
              (\par se -> mkLet [paramIdent par] (BasicOp $ SubExp se))
              accpars
              accs
          plus_bdy = lambdaBody plus
          newlambdy =
            Body
              (bodyDec plus_bdy)
              (stmsFromList parstms <> bodyStms plus_bdy)
              (bodyResult plus_bdy)
      renameLambda $ Lambda rempars newlambdy $ lambdaReturnType plus

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
