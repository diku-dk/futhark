{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- | This module contains a representation for the index function based on
-- linear-memory accessor descriptors; see Zhu, Hoeflinger and David work.
module Futhark.Representation.ExplicitMemory.IndexFunction
       ( IxFun(..)
       , index
       , iota
       , offsetIndex
       , permute
       , rotate
       , reshape
       , slice
       , rebase
       , repeat
       , shape
       , rank
       , linearWithOffset
       , rearrangeWithOffset
       , isDirect
       , isLinear
       , substituteInIxFun
       , leastGeneralGeneralization
       , closeEnough
       )
       where

import Prelude hiding (mod, repeat)
import Data.List hiding (repeat)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Function (on)
import Data.Maybe (isJust)
import Control.Monad.Identity
import Control.Monad.Writer
import qualified Data.Map.Strict as M

import Futhark.Analysis.PrimExp (PrimExp(..))
import Futhark.Representation.AST.Syntax.Core (Ext(..))
import Futhark.Transform.Substitute
import Futhark.Transform.Rename
import Futhark.Representation.AST.Syntax
  (ShapeChange, DimChange(..), DimIndex(..), Slice, unitSlice, dimFix)
import Futhark.Representation.AST.Attributes
import Futhark.Util.IntegralExp
import Futhark.Util.Pretty
import Futhark.Analysis.PrimExp.Convert (substituteInPrimExp)
import qualified Futhark.Analysis.PrimExp.Generalize as PEG

-- | LMAD's representation consists of a general offset and for each dimension a
-- stride, rotate factor, number of elements (or shape), permutation, and
-- monotonicity. Note that the permutation is not strictly necessary in that the
-- permutation can be performed directly on LMAD dimensions, but then it is
-- difficult to extract the permutation back from an LMAD.
--
-- LMAD algebra is closed under composition w.r.t. operators such as permute,
-- repeat, index and slice.  However, other operations, such as reshape, cannot
-- always be represented inside the LMAD algebra.
--
-- It follows that the general representation of an index function is a list of
-- LMADS, in which each following LMAD in the list implicitly corresponds to an
-- irregular reshaping operation.
--
-- However, we expect that the common case is when the index function is one
-- LMAD -- we call this the 'nice' representation.
--
-- Finally, the list of LMADs is kept in an @IxFun@ together with the shape of
-- the original array, and a bit to indicate whether the index function is
-- contiguous, i.e., if we instantiate all the points of the current index
-- function, do we get a contiguous memory interval?
--
-- By definition, the LMAD denotes the set of points (simplified):
--
--   \{ o + \Sigma_{j=0}^{k} ((i_j+r_j) `mod` n_j)*s_j,
--      \forall i_j such that 0<=i_j<n_j, j=1..k \}
type Shape num   = [num]
type Indices num = [num]
type Permutation = [Int]

data Monotonicity = Inc | Dec | Unknown
               -- ^ monotonously increasing, decreasing or unknown
             deriving (Show, Eq)

data LMADDim num = LMADDim { ldStride :: num
                           , ldRotate :: num
                           , ldShape :: num
                           , ldPerm :: Int
                           , ldMon :: Monotonicity
                           }
                 deriving (Show, Eq)

data LMAD num = LMAD { lmadOffset :: num
                     , lmadDims :: [LMADDim num]
                     }
                deriving (Show, Eq)

data IxFun num = IxFun { ixfunLMADs :: NonEmpty (LMAD num)
                       , base :: Shape num
                       , ixfunContig :: Bool
                       -- ^ ignoring permutations, is the index function contiguous?
                       }
                 deriving (Show, Eq)


instance Pretty Monotonicity where
  ppr = text . show

instance Pretty num => Pretty (LMAD num) where
  ppr (LMAD offset dims) =
    braces $ semisep [ text "offset: " <> oneLine (ppr offset)
                     , text "strides: " <> p ldStride
                     , text "rotates: " <> p ldRotate
                     , text "shape: " <> p ldShape
                     , text "permutation: " <> p ldPerm
                     , text "monotonicity: " <> p ldMon
                     ]
    where p f = oneLine $ brackets $ commasep $ map (ppr . f) dims

instance Pretty num => Pretty (IxFun num) where
  ppr (IxFun lmads oshp cg) =
    braces $ semisep [ text "base: " <> brackets (commasep $ map ppr oshp)
                     , text "contiguous: " <> text (show cg)
                     , text "LMADs: " <> brackets (commasep $ NE.toList $ NE.map ppr lmads)
                     ]


instance Substitute num => Substitute (LMAD num) where
  substituteNames substs = fmap $ substituteNames substs

instance Substitute num => Substitute (IxFun num) where
  substituteNames substs = fmap $ substituteNames substs

instance Substitute num => Rename (LMAD num) where
  rename = substituteRename

instance Substitute num => Rename (IxFun num) where
  rename = substituteRename


instance FreeIn num => FreeIn (LMAD num) where
  freeIn' = foldMap freeIn'

instance FreeIn num => FreeIn (IxFun num) where
  freeIn' = foldMap freeIn'

instance Functor LMAD where
  fmap f = runIdentity . traverse (return . f)

instance Functor IxFun where
  fmap f = runIdentity . traverse (return . f)


instance Foldable LMAD where
  foldMap f = execWriter . traverse (tell . f)

instance Foldable IxFun where
  foldMap f = execWriter . traverse (tell . f)


instance Traversable LMAD where
  traverse f (LMAD offset dims) =
    LMAD <$> f offset <*> traverse f' dims
    where f' (LMADDim s r n p m) =
             LMADDim <$> f s <*> f r <*> f n <*> pure p <*> pure m

instance Traversable IxFun where
  traverse f (IxFun lmads oshp cg) =
    IxFun  <$> traverse (traverse f) lmads <*> traverse f oshp <*> pure cg

(++@) :: [a] -> NonEmpty a -> NonEmpty a
es ++@ (ne :| nes) = case es of
  e : es' -> e :| es' ++ [ne] ++ nes
  [] -> ne :| nes

(@++@) :: NonEmpty a -> NonEmpty a -> NonEmpty a
(x :| xs) @++@ (y :| ys) = x :| xs ++ [y] ++ ys

invertMonotonicity :: Monotonicity -> Monotonicity
invertMonotonicity Inc = Dec
invertMonotonicity Dec = Inc
invertMonotonicity Unknown = Unknown

lmadPermutation :: LMAD num -> Permutation
lmadPermutation = map ldPerm . lmadDims

setLMADPermutation :: Permutation -> LMAD num -> LMAD num
setLMADPermutation perm lmad =
  lmad { lmadDims = zipWith (\dim p -> dim { ldPerm = p }) (lmadDims lmad) perm }

setLMADShape :: Shape num -> LMAD num -> LMAD num
setLMADShape shp lmad = lmad { lmadDims = zipWith (\dim s -> dim { ldShape = s }) (lmadDims lmad) shp }

-- | Substitute a name with a PrimExp in an LMAD.
substituteInLMAD :: Ord a => M.Map a (PrimExp a) -> LMAD (PrimExp a)
                 -> LMAD (PrimExp a)
substituteInLMAD tab (LMAD offset dims) =
  let offset' = substituteInPrimExp tab offset
      dims' = map (\(LMADDim s r n p m) ->
                     LMADDim
                     (substituteInPrimExp tab s)
                     (substituteInPrimExp tab r)
                     (substituteInPrimExp tab n)
                     p m)
              dims
  in LMAD offset' dims'

-- | Substitute a name with a PrimExp in an index function.
substituteInIxFun :: (Ord a) => M.Map a (PrimExp a) -> IxFun (PrimExp a)
                  -> IxFun (PrimExp a)
substituteInIxFun tab (IxFun lmads oshp cg) =
  IxFun (NE.map (substituteInLMAD tab) lmads)
        (map (substituteInPrimExp tab) oshp)
        cg

-- | Is this is a row-major array?
isDirect :: (Eq num, IntegralExp num) => IxFun num -> Bool
isDirect ixfun@(IxFun (LMAD offset dims :| []) oshp True) =
  let strides_expected = reverse $ scanl (*) 1 (reverse (tail oshp))
  in hasContiguousPerm ixfun &&
     length oshp == length dims &&
     offset == 0 &&
     all (\(LMADDim s r n p _, m, d, se) ->
            s == se && r == 0 && n == d && p == m)
     (zip4 dims [0..length dims - 1] oshp strides_expected)
isDirect _ = False

-- | Does the index function have an ascending permutation?
hasContiguousPerm :: IxFun num -> Bool
hasContiguousPerm (IxFun (lmad :| []) _ _) =
  let perm = lmadPermutation lmad
  in perm == sort perm
hasContiguousPerm _ = False

-- | Shape of an index function.
shape :: (Eq num, IntegralExp num) => IxFun num -> Shape num
shape (IxFun (lmad :| _) _ _) = lmadShape lmad

-- | Shape of an LMAD.
lmadShape :: (Eq num, IntegralExp num) => LMAD num -> Shape num
lmadShape lmad = permuteInv (lmadPermutation lmad) $ lmadShapeBase lmad

-- | Shape of an LMAD, ignoring permutations.
lmadShapeBase :: (Eq num, IntegralExp num) => LMAD num -> Shape num
lmadShapeBase = map ldShape . lmadDims

-- | Compute the flat memory index for a complete set `inds` of array indices
-- and a certain element size `elem_size`.
index :: (IntegralExp num, Eq num) =>
          IxFun num -> Indices num -> num
index = indexFromLMADs . ixfunLMADs
  where indexFromLMADs :: (IntegralExp num, Eq num) =>
                          NonEmpty (LMAD num) -> Indices num -> num
        indexFromLMADs (lmad :| []) inds = indexLMAD lmad inds
        indexFromLMADs (lmad1 :| lmad2 : lmads) inds =
          let i_flat   = indexLMAD lmad1 inds
              new_inds = unflattenIndex (permuteFwd (lmadPermutation lmad2) $ lmadShapeBase lmad2) i_flat
          in indexFromLMADs (lmad2 :| lmads) new_inds

        -- | Compute the flat index of an LMAD.
        indexLMAD :: (IntegralExp num, Eq num) =>
                     LMAD num -> Indices num -> num
        indexLMAD lmad@(LMAD off dims) inds =
          let prod = sum $ zipWith flatOneDim
                             (map (\(LMADDim s r n _ _) -> (s, r, n)) dims)
                             (permuteInv (lmadPermutation lmad) inds)
          in off + prod

-- | iota.
iota :: IntegralExp num => Shape num -> IxFun num
iota ns =
  let rs = replicate (length ns) 0
  in IxFun (makeRotIota Inc 0 (zip rs ns) :| []) ns True

-- | Permute dimensions.
permute :: IntegralExp num =>
           IxFun num -> Permutation -> IxFun num
permute (IxFun (lmad :| lmads) oshp cg) perm_new =
  let perm_cur = lmadPermutation lmad
      perm = map (perm_cur !!) perm_new
  in IxFun (setLMADPermutation perm lmad :| lmads) oshp cg

-- | Repeat dimensions.
repeat :: (Eq num, IntegralExp num) =>
          IxFun num -> [Shape num] -> Shape num -> IxFun num
repeat (IxFun (lmad@(LMAD off dims) :| lmads) oshp _) shps shp =
  let perm = lmadPermutation lmad
      -- inverse permute the shapes and update the permutation
      lens = map (\s -> 1 + length s) shps
      (shps', lens') = unzip $ permuteInv perm $ zip shps lens
      scn = drop 1 $ scanl (+) 0 lens'
      perm' = concatMap (\(p, l) -> map (\i-> (scn !! p) - l + i) [0..l-1])
                        $ zip perm lens
      tmp = length perm'
      perm'' = perm' ++ [tmp..tmp-1+length shp]

      dims' = concatMap (\(shp_k, srnp) ->
                            map fakeDim shp_k ++ [srnp]
                        ) $ zip shps' dims
      lmad' = setLMADPermutation perm'' $ LMAD off (dims' ++ map fakeDim shp)
  in IxFun (lmad' :| lmads) oshp False -- XXX: Can we be less conservative?
  where fakeDim x = LMADDim 0 0 x 0 Unknown

-- | Rotate an index function.
rotate :: (Eq num, IntegralExp num) =>
          IxFun num -> Indices num -> IxFun num
rotate  (IxFun (lmad@(LMAD off dims) :| lmads) oshp cg) offs =
  let dims' = zipWith (\(LMADDim s r n p f) o ->
                          if s == 0 then LMADDim 0 0 n p Unknown
                          else LMADDim s (r + o) n p f
                      ) dims (permuteInv (lmadPermutation lmad) offs)
  in IxFun (LMAD off dims' :| lmads) oshp cg

-- | Handle the case where a slice can stay within a single LMAD.
sliceOneLMAD :: (Eq num, IntegralExp num) =>
                IxFun num -> Slice num -> Maybe (IxFun num)
sliceOneLMAD (IxFun (lmad@(LMAD _ ldims) :| lmads) oshp cg) is = do
  let perm = lmadPermutation lmad
      is' = permuteInv perm is
      cg' = cg && slicePreservesContiguous lmad is'
  guard $ harmlessRotation lmad is'
  let lmad' = foldl sliceOne (LMAD (lmadOffset lmad) []) $ zip is' ldims
      -- need to remove the fixed dims from the permutation
      perm' = updatePerm perm $ map fst $ filter (isJust . dimFix . snd) $
              zip [0..length is' - 1] is'

  return $ IxFun (setLMADPermutation perm' lmad' :| lmads) oshp cg'
  where updatePerm ps inds = foldl (\acc p -> acc ++ decrease p) [] ps
          where decrease p =
                  let d = foldl (\n i -> if i == p then -1
                                         else if i > p
                                              then n
                                              else if n /= -1 then n + 1
                                                   else n
                                ) 0 inds
                  in [p - d | d /= -1]

        harmlessRotation' :: (Eq num, IntegralExp num) =>
                             LMADDim num -> DimIndex num -> Bool
        harmlessRotation' _ (DimFix _)   = True
        harmlessRotation' (LMADDim 0 _ _ _ _) _  = True
        harmlessRotation' (LMADDim _ 0 _ _ _) _  = True
        harmlessRotation' (LMADDim _ _ n _ _) dslc
            | dslc == DimSlice (n - 1) n (-1) ||
              dslc == unitSlice 0 n      = True
        harmlessRotation' _ _            = False

        harmlessRotation :: (Eq num, IntegralExp num) =>
                             LMAD num -> Slice num -> Bool
        harmlessRotation (LMAD _ dims) iss =
            and $ zipWith harmlessRotation' dims iss

        -- XXX: TODO: what happens to r on a negative-stride slice; is there
        -- such a case?
        sliceOne :: (Eq num, IntegralExp num) =>
                    LMAD num -> (DimIndex num, LMADDim num) -> LMAD num
        sliceOne (LMAD off dims) (DimFix i, LMADDim s r n _ _) =
            LMAD (off + flatOneDim (s, r, n) i) dims
        sliceOne (LMAD off dims) (DimSlice _ ne _, LMADDim 0 _ _ p _) =
            LMAD off (dims ++ [LMADDim 0 0 ne p Unknown])
        sliceOne (LMAD off dims) (dmind, dim@(LMADDim _ _ n _ _))
            | dmind == unitSlice 0 n = LMAD off (dims ++ [dim])
        sliceOne (LMAD off dims) (dmind, LMADDim s r n p m)
            | dmind == DimSlice (n - 1) n (-1) =
              let r' = if r == 0 then 0 else n - r
                  off' = off + flatOneDim (s, 0, n) (n - 1)
              in  LMAD off' (dims ++ [LMADDim (s * (-1)) r' n p (invertMonotonicity m)])
        sliceOne (LMAD off dims) (DimSlice b ne 0, LMADDim s r n p _) =
            LMAD (off + flatOneDim (s, r, n) b) (dims ++ [LMADDim 0 0 ne p Unknown])
        sliceOne (LMAD off dims) (DimSlice bs ns ss, LMADDim s 0 _ p m) =
            let m' = case sgn ss of
                       Just 1    -> m
                       Just (-1) -> invertMonotonicity m
                       _         -> Unknown
            in  LMAD (off + s * bs) (dims ++ [LMADDim (ss * s) 0 ns p m'])
        sliceOne _ _ = error "slice: reached impossible case"

        slicePreservesContiguous :: (Eq num, IntegralExp num) =>
                                    LMAD num -> Slice num -> Bool
        slicePreservesContiguous (LMAD _ dims) slc =
          -- remove from the slice the LMAD dimensions that have stride 0.
          -- If the LMAD was contiguous in mem, then these dims will not
          -- influence the contiguousness of the result.
          -- Also normalize the input slice, i.e., 0-stride and size-1
          -- slices are rewritten as DimFixed.
          let (dims', slc') = unzip $
                filter ((/= 0) . ldStride . fst) $
                       zip dims $ map normIndex slc
              -- Check that:
              -- 1. a clean split point exists between Fixed and Sliced dims
              -- 2. the outermost sliced dim has +/- 1 stride AND is unrotated or full.
              -- 3. the rest of inner sliced dims are full.
              (_, success) =
                foldl (\(found, res) (slcdim, LMADDim _ r n _ _) ->
                        case (slcdim, found) of
                          (DimFix{},   True ) -> (found, False)
                          (DimFix{},   False) -> (found, res)
                          (DimSlice _ ne ds, False) -> -- outermost sliced dim: +/-1 stride
                            let res' = (r == 0 || n == ne) && (ds == 1 || ds == -1)
                            in (True, res && res')
                          (DimSlice _ ne ds, True) ->  -- inner sliced dim: needs to be full
                            let res' = (n == ne) && (ds == 1 || ds == -1)
                            in (found, res && res')
                      ) (False, True) $ zip slc' dims'
          in success

        normIndex :: (Eq num, IntegralExp num) =>
                     DimIndex num -> DimIndex num
        normIndex (DimSlice b 1 _) = DimFix b
        normIndex (DimSlice b _ 0) = DimFix b
        normIndex d = d

-- | Slice an index function.
slice :: (Eq num, IntegralExp num) =>
         IxFun num -> Slice num -> IxFun num
slice _ [] = error "slice: empty slice"
slice ixfun@(IxFun (lmad@(LMAD _ _) :| lmads) oshp cg) dim_slices
  -- Avoid identity slicing.
  | dim_slices == map (unitSlice 0) (shape ixfun) = ixfun
  | Just ixfun' <- sliceOneLMAD ixfun dim_slices = ixfun'
  | otherwise =
    case sliceOneLMAD (iota (lmadShape lmad)) dim_slices of
      Just (IxFun (lmad' :| []) _ cg') ->
        IxFun (lmad' :| lmad : lmads) oshp (cg && cg')
      _ -> error "slice: reached impossible case"

-- | Handle the simple case where all reshape dimensions are coercions.
reshapeCoercion :: (Eq num, IntegralExp num) =>
                   IxFun num -> ShapeChange num -> Maybe (IxFun num)
reshapeCoercion (IxFun (lmad@(LMAD off dims) :| lmads) _ cg) newshape = do
  let perm = lmadPermutation lmad
  (head_coercions, reshapes, tail_coercions) <- splitCoercions newshape
  let hd_len = length head_coercions
      num_coercions = hd_len + length tail_coercions
      dims' = permuteFwd perm dims
      mid_dims = take (length dims - num_coercions) $ drop hd_len dims'
      num_rshps = length reshapes
  guard (num_rshps == 0 || (num_rshps == 1 && length mid_dims == 1))
  let dims'' = map snd $ sortBy (compare `on` fst) $
               zipWith (\ld n -> (ldPerm ld, ld { ldShape = n }))
               dims' (newDims newshape)
      lmad' = LMAD off dims''
  return $ IxFun (lmad' :| lmads) (newDims newshape) cg

-- | Handle the case where a reshape operation can stay inside a single LMAD.
--
-- There are four conditions that all must hold for the result of a reshape
-- operation to remain in the one-LMAD domain:
--
--   (1) the permutation of the underlying LMAD must leave unchanged
--       the LMAD dimensions that were *not* reshape coercions.
--   (2) the repetition of dimensions of the underlying LMAD must
--       refer only to the coerced-dimensions of the reshape operation.
--   (3) similarly, the rotated dimensions must refer only to
--       dimensions that are coerced by the reshape operation.
--   (4) finally, the underlying memory is contiguous (and monotonous).
--
-- If any of these conditions do not hold, then the reshape operation will
-- conservatively add a new LMAD to the list, leading to a representation that
-- provides less opportunities for further analysis.
reshapeOneLMAD :: (Eq num, IntegralExp num) =>
                   IxFun num -> ShapeChange num -> Maybe (IxFun num)
reshapeOneLMAD ixfun@(IxFun (lmad@(LMAD off dims) :| lmads) _ cg) newshape = do
  let perm = lmadPermutation lmad
  (head_coercions, reshapes, tail_coercions) <- splitCoercions newshape
  let hd_len = length head_coercions
      num_coercions = hd_len + length tail_coercions
      dims_perm = permuteFwd perm dims
      mid_dims = take (length dims - num_coercions) $ drop hd_len dims_perm
      -- Ignore rotates, as we only care about not having rotates in the
      -- dimensions that aren't coercions (@mid_dims@), which we check
      -- separately.
      mon = ixfunMonotonicityRots True ixfun

  guard $
    -- checking conditions (2) and (3)
    all (\ (LMADDim s r _ _ _) -> s /= 0 && r == 0) mid_dims &&
    -- checking condition (1)
    consecutive hd_len (map ldPerm mid_dims) &&
    -- checking condition (4)
    hasContiguousPerm ixfun && cg && (mon == Inc || mon == Dec)

  -- make new permutation
  let rsh_len = length reshapes
      diff = length newshape - length dims
      iota_shape = [0..length newshape-1]
      perm' = map (\i -> let ind = if i < hd_len
                                   then i else i - diff
                         in if (i >= hd_len) && (i < hd_len + rsh_len)
                            then i -- already checked mid_dims not affected
                            else let p = ldPerm (dims !! ind)
                                 in if p < hd_len
                                    then p
                                    else p + diff
                  ) iota_shape
      -- split the dimensions
      (support_inds, repeat_inds) =
        foldl (\(sup, rpt) (i, shpdim, ip) ->
                case (i < hd_len, i >= hd_len + rsh_len, shpdim) of
                  (True,  _, DimCoercion n) ->
                    case dims_perm !! i of
                      (LMADDim 0 _ _ _ _) -> ( sup, (ip, n) : rpt )
                      (LMADDim _ r _ _ _) -> ( (ip, (r, n)) : sup, rpt )
                  (_,  True, DimCoercion n) ->
                    case dims_perm !! (i-diff) of
                      (LMADDim 0 _ _ _ _) -> ( sup, (ip, n) : rpt )
                      (LMADDim _ r _ _ _) -> ( (ip, (r, n)) : sup, rpt )
                  (False, False, _) ->
                      ( (ip, (0, newDim shpdim)) : sup, rpt )
                      -- already checked that the reshaped
                      -- dims cannot be repeats or rotates
                  _ -> error "reshape: reached impossible case"
              ) ([], []) $ reverse $ zip3 iota_shape newshape perm'

      (sup_inds, support) = unzip $ sortBy (compare `on` fst) support_inds
      (rpt_inds, repeats) = unzip repeat_inds
      LMAD off' dims_sup = makeRotIota mon off support
      repeats' = map (\n -> LMADDim 0 0 n 0 Unknown) repeats
      dims' = map snd $ sortBy (compare `on` fst)
              $ zip sup_inds dims_sup ++ zip rpt_inds repeats'
      lmad' = LMAD off' dims'
  return $ IxFun (setLMADPermutation perm' lmad' :| lmads) (newDims newshape) cg
  where consecutive _ [] = True
        consecutive i [p]= i == p
        consecutive i ps = and $ zipWith (==) ps [i, i+1..]

splitCoercions :: (Eq num, IntegralExp num) =>
                  ShapeChange num -> Maybe (ShapeChange num, ShapeChange num, ShapeChange num)
splitCoercions newshape' = do
  let (head_coercions, newshape'') = span isCoercion newshape'
      (reshapes, tail_coercions) = break isCoercion newshape''
  guard (all isCoercion tail_coercions)
  return (head_coercions, reshapes, tail_coercions)
  where isCoercion DimCoercion{} = True
        isCoercion _ = False

-- | Reshape an index function.
reshape :: (Eq num, IntegralExp num) =>
           IxFun num -> ShapeChange num -> IxFun num
reshape ixfun new_shape
  | Just ixfun' <- reshapeCoercion ixfun new_shape = ixfun'
  | Just ixfun' <- reshapeOneLMAD ixfun new_shape = ixfun'
reshape (IxFun (lmad0 :| lmad0s) oshp cg) new_shape =
  case iota (newDims new_shape) of
    IxFun (lmad :| []) _ _ -> IxFun (lmad :| lmad0 : lmad0s) oshp cg
    _ -> error "reshape: reached impossible case"

rank :: IntegralExp num =>
        IxFun num -> Int
rank (IxFun (LMAD _ sss :| _) _ _) = length sss

-- | Handle the case where a rebase operation can stay within m + n - 1 LMADs,
-- where m is the number of LMADs in the index function, and n is the number of
-- LMADs in the new base.  If both index function have only on LMAD, this means
-- that we stay within the single-LMAD domain.
--
-- We can often stay in that domain if the original ixfun is essentially a
-- slice, e.g. `x[i, (k1,m,s1), (k2,n,s2)] = orig`.
--
-- XXX: TODO: handle repetitions in both lmads.
--
-- How to handle repeated dimensions in the original?
--
--   (a) Shave them off of the last lmad of original
--   (b) Compose the result from (a) with the first
--       lmad of the new base
--   (c) apply a repeat operation on the result of (b).
--
-- However, I strongly suspect that for in-place update what we need is actually
-- the INVERSE of the rebase function, i.e., given an index function new-base
-- and another one orig, compute the index function ixfun0 such that:
--
--   new-base == rebase ixfun0 ixfun, or equivalently:
--   new-base == ixfun o ixfun0
--
-- because then I can go bottom up and compose with ixfun0 all the index
-- functions corresponding to the memory block associated with ixfun.
rebaseNice :: (Eq num, IntegralExp num) =>
              IxFun num -> IxFun num -> Maybe (IxFun num)
rebaseNice
  new_base@(IxFun (lmad_base :| lmads_base) _ cg_base)
  ixfun@(IxFun lmads shp cg) = do
  let (lmad_full :| lmads') = NE.reverse lmads
      ((outer_shapes, inner_shape), lmad) = shaveoffRepeats lmad_full
      dims = lmadDims lmad
      perm = lmadPermutation lmad
      perm_base = lmadPermutation lmad_base

  guard $
    -- Core rebase condition.
    base ixfun == shape new_base
    -- Conservative safety conditions: ixfun is contiguous and has known
    -- monotonicity for all dimensions.
    && cg && all ((/= Unknown) . ldMon) dims
    -- XXX: We should be able to handle some basic cases where both index
    -- functions have non-trivial permutations.
    && (hasContiguousPerm ixfun || hasContiguousPerm new_base)
    -- We need the permutations to be of the same size if we want to compose
    -- them.  They don't have to be of the same size if the ixfun has a trivial
    -- permutation.  Supporting this latter case allows us to rebase when ixfun
    -- has been created by slicing with fixed dimensions.
    && (length perm == length perm_base || hasContiguousPerm ixfun)
    -- To not have to worry about ixfun having non-1 strides, we also check that
    -- it is a row-major array (modulo permutation, which is handled
    -- separately).  Accept a non-full innermost dimension.  XXX: Maybe this can
    -- be less conservative?
    && and (zipWith3 (\sn ld inner -> sn == ldShape ld || (inner && ldStride ld == 1))
            shp dims (replicate (length dims - 1) False ++ [True]))

  -- Compose permutations, reverse strides and adjust offset if necessary.
  let perm_base' = if hasContiguousPerm ixfun
                   then perm_base
                   else map (perm !!) perm_base
      lmad_base' = setLMADPermutation perm_base' lmad_base
      dims_base = lmadDims lmad_base'
      n_fewer_dims = length dims_base - length dims
      (dims_base', offs_contrib) = unzip $
        zipWith (\(LMADDim s1 r1 n1 p1 _) (LMADDim _ r2 _ _ m2) ->
                   let (s', off') | m2 == Inc = (s1, 0)
                                  | otherwise = (s1 * (-1), s1 * (n1 - 1))
                       r' | m2 == Inc = if r2 == 0 then r1 else r1 + r2
                          | r1 == 0 = r2
                          | r2 == 0 = n1 - r1
                          | otherwise = n1 - r1 + r2
                   in (LMADDim s' r' n1 (p1 - n_fewer_dims) Inc, off'))
        -- If @dims@ is morally a slice, it might have fewer dimensions than
        -- @dims_base@.  Drop extraneous outer dimensions.
        (drop n_fewer_dims dims_base) dims
      off_base = lmadOffset lmad_base' + sum offs_contrib
      lmad_base''
        | lmadOffset lmad == 0 = LMAD off_base dims_base'
        | otherwise =
            -- If the innermost dimension of the ixfun was not full (but still
            -- had a stride of 1), add its offset relative to the new base.
            setLMADShape (lmadShape lmad)
            (LMAD (off_base + ldStride (last dims_base) * lmadOffset lmad)
             dims_base')
      new_base' = IxFun (lmad_base'' :| lmads_base) shp cg_base
      IxFun lmads_base' _ _ = if all null outer_shapes && null inner_shape
                              then new_base'
                              else repeat new_base' outer_shapes inner_shape
      lmads'' = lmads' ++@ lmads_base'
  return $ IxFun lmads'' shp (cg && cg_base)
  where shaveoffRepeats :: (Eq num, IntegralExp num) =>
                           LMAD num -> (([Shape num], Shape num), LMAD num)
        shaveoffRepeats lmad =
        -- Given an input lmad, this function computes a repetition @r@ and a new lmad
        -- @res@, such that @repeat r res@ is identical to the input lmad.
          let perm = lmadPermutation lmad
              dims = lmadDims lmad
              -- compute the Repeat:
              resacc= foldl (\acc (LMADDim s _ n _ _) ->
                              case acc of
                                rpt : acc0 ->
                                    if s == 0 then (n : rpt) : acc0
                                    else [] : (rpt : acc0)
                                _ -> error "shaveoffRepeats: empty accumulator"
                            ) [[]] $ reverse $ permuteFwd perm dims
              last_shape = last resacc
              shapes = take (length resacc - 1) resacc
              -- update permutation and lmad:
              howManyRepLT k =
                foldl (\i (LMADDim s _ _ p _) ->
                         if s == 0 && p < k then i + 1 else i
                      ) 0 dims
              dims' = foldl (\acc (LMADDim s r n p info) ->
                               if s == 0 then acc
                               else let p' = p - howManyRepLT p
                                    in LMADDim s r n p' info : acc
                             ) [] $ reverse dims
              lmad' = LMAD (lmadOffset lmad) dims'
          in ((shapes, last_shape), lmad')

-- | Rebase an index function on top of a new base.
rebase :: (Eq num, IntegralExp num) =>
          IxFun num -> IxFun num -> IxFun num
rebase new_base@(IxFun lmads_base shp_base cg_base) ixfun@(IxFun lmads shp cg)
  | Just ixfun' <- rebaseNice new_base ixfun = ixfun'
  -- In the general case just concatenate LMADs since this refers to index
  -- function composition, which is always safe.
  | otherwise =
      let (lmads_base', shp_base') =
            if base ixfun == shape new_base
            then (lmads_base, shp_base)
            else let IxFun lmads' shp_base'' _ = reshape new_base $ map DimCoercion shp
                 in (lmads', shp_base'')
      in IxFun (lmads @++@ lmads_base') shp_base' (cg && cg_base)

ixfunMonotonicity :: (Eq num, IntegralExp num) => IxFun num -> Monotonicity
ixfunMonotonicity = ixfunMonotonicityRots False

-- | Offset index.  Results in the index function corresponding to indexing with
-- @i@ on the outermost dimension.
offsetIndex :: (Eq num, IntegralExp num) =>
               IxFun num -> num -> IxFun num
offsetIndex ixfun i | i == 0 = ixfun
offsetIndex ixfun i =
  case shape ixfun of
    d : ds -> slice ixfun (DimSlice i (d - i) 1 : map (unitSlice 0) ds)
    [] -> error "offsetIndex: underlying index function has rank zero"

-- | If the memory support of the index function is contiguous and row-major
-- (i.e., no transpositions, repetitions, rotates, etc.), then this should
-- return the offset from which the memory-support of this index function
-- starts.
linearWithOffset :: (Eq num, IntegralExp num) =>
                    IxFun num -> num -> Maybe num
linearWithOffset ixfun@(IxFun (lmad :| []) _ cg) elem_size
  | hasContiguousPerm ixfun && cg && ixfunMonotonicity ixfun == Inc =
    Just $ lmadOffset lmad * elem_size
linearWithOffset _ _ = Nothing

-- | Similar restrictions to @linearWithOffset@ except for transpositions, which
-- are returned together with the offset.
rearrangeWithOffset :: (Eq num, IntegralExp num) =>
                       IxFun num -> num -> Maybe (num, [(Int,num)])
rearrangeWithOffset (IxFun (lmad :| []) oshp cg) elem_size = do
  -- Note that @cg@ describes whether the index function is
  -- contiguous, *ignoring permutations*.  This function requires that
  -- functionality.
  let perm = lmadPermutation lmad
      perm_contig = [0..length perm-1]
  offset <- linearWithOffset
            (IxFun (setLMADPermutation perm_contig lmad :| []) oshp cg) elem_size
  return (offset, zip perm (permuteFwd perm (lmadShapeBase lmad)))
rearrangeWithOffset _ _ = Nothing

isLinear :: (Eq num, IntegralExp num) => IxFun num -> Bool
isLinear = (== Just 0) . flip linearWithOffset 1

permuteFwd :: Permutation -> [a] -> [a]
permuteFwd ps elems = map (elems !!) ps

permuteInv :: Permutation -> [a] -> [a]
permuteInv ps elems = map snd $ sortBy (compare `on` fst) $ zip ps elems

flatOneDim :: (Eq num, IntegralExp num) =>
              (num, num, num) -> num -> num
flatOneDim (s, r, n) i
  | s == 0 = 0
  | r == 0 = i * s
  | otherwise = ((i + r) `mod` n) * s

-- | Generalised iota with user-specified offset and strides.
makeRotIota :: IntegralExp num =>
               Monotonicity -> num -> [(num, num)] -> LMAD num
makeRotIota mon off support
  | mon == Inc || mon == Dec =
    let rk = length support
        (rs, ns) = unzip support
        ss0 = reverse $ take rk $ scanl (*) 1 $ reverse ns
        ss = if mon == Inc
             then ss0
             else map (* (-1)) ss0
        ps = map fromIntegral [0..rk-1]
        fi = replicate rk mon
    in LMAD off $ zipWith5 LMADDim ss rs ns ps fi
  | otherwise = error "makeRotIota: requires Inc or Dec"

-- | Check monotonicity of an index function.
ixfunMonotonicityRots :: (Eq num, IntegralExp num) =>
                         Bool -> IxFun num -> Monotonicity
ixfunMonotonicityRots ignore_rots (IxFun (lmad :| lmads) _ _) =
  let mon0 = lmadMonotonicityRots lmad
  in if all ((== mon0) . lmadMonotonicityRots) lmads
     then mon0
     else Unknown
  where lmadMonotonicityRots :: (Eq num, IntegralExp num) =>
                                LMAD num -> Monotonicity
        lmadMonotonicityRots (LMAD _ dims)
          | all (isMonDim Inc) dims = Inc
          | all (isMonDim Dec) dims = Dec
          | otherwise = Unknown

        isMonDim :: (Eq num, IntegralExp num) =>
                    Monotonicity -> LMADDim num -> Bool
        isMonDim mon (LMADDim s r _ _ ldmon) =
          s == 0 || ((ignore_rots || r == 0) && mon == ldmon)

-- | Generalization (anti-unification)
--
-- Anti-unification of two index functions is supported under the following conditions:
--   0. Both index functions are represented by ONE lmad (assumed common case!)
--   1. The support array of the two indexfuns have the same dimensionality
--      (we can relax this condition if we use a 1D support, as we probably should!)
--   2. The contiguous property and the per-dimension monotonicity are the same
--      (otherwise we might loose important information; this can be relaxed!)
--   3. Most importantly, both index functions correspond to the same permutation
--      (since the permutation is represented by INTs, this restriction cannot
--       be relaxed, unless we move to a gated-LMAD representation!)
--
-- `k0` is the existential to use for the shape of the array.
leastGeneralGeneralization :: Eq v => IxFun (PrimExp v) -> IxFun (PrimExp v) ->
                              Maybe (IxFun (PrimExp (Ext v)), [(PrimExp v, PrimExp v)])
leastGeneralGeneralization (IxFun (lmad1 :| []) oshp1 ctg1) (IxFun (lmad2 :| []) oshp2 ctg2) = do
  guard $
    length oshp1 == length oshp2 &&
    ctg1 == ctg2 &&
    map ldPerm (lmadDims lmad1) == map ldPerm (lmadDims lmad2) &&
    lmadDMon lmad1 == lmadDMon lmad2
  let (ctg, dperm, dmon) = (ctg1, lmadPermutation lmad1, lmadDMon lmad1)
  (dshp, m1) <- generalize [] (lmadDShp lmad1) (lmadDShp lmad2)
  (oshp, m2) <- generalize m1 oshp1 oshp2
  (dstd, m3) <- generalize m2 (lmadDSrd lmad1) (lmadDSrd lmad2)
  (drot, m4) <- generalize m3 (lmadDRot lmad1) (lmadDRot lmad2)
  (offt, m5) <- PEG.leastGeneralGeneralization m4 (lmadOffset lmad1) (lmadOffset lmad2)
  let lmad_dims = map (\(a,b,c,d,e) -> LMADDim a b c d e) $
        zip5 dstd drot dshp dperm dmon
      lmad = LMAD offt lmad_dims
  return (IxFun (lmad :| []) oshp ctg, m5)
  where lmadDMon = map ldMon    . lmadDims
        lmadDSrd = map ldStride . lmadDims
        lmadDShp = map ldShape  . lmadDims
        lmadDRot = map ldRotate . lmadDims
        generalize m l1 l2 =
          foldM (\(l_acc, m') (pe1,pe2) -> do
                    (e, m'') <- PEG.leastGeneralGeneralization m' pe1 pe2
                    return (l_acc++[e], m'')
                ) ([], m) (zip l1 l2)
leastGeneralGeneralization _ _ = Nothing

-- | When comparing index functions as part of the type check in ExplicitMemory,
-- we may run into problems caused by the simplifier. As index functions can be
-- generalized over if-then-else expressions, the simplifier might hoist some of
-- the code from inside the if-then-else (computing the offset of an array, for
-- instance), but now the type checker cannot verify that the generalized index
-- function is valid, because some of the existentials are computed somewhere
-- else. To Work around this, we've had to relax the ExplicitMemory type-checker
-- a bit, specifically, we've introduced this function to verify whether two
-- index functions are "close enough" that we can assume that they match. We use
-- this instead of `ixfun1 == ixfun2` and hope that it's good enough.
closeEnough :: IxFun num -> IxFun num -> Bool
closeEnough ixf1 ixf2 =
  (length (base ixf1) == length (base ixf2)) &&
  (ixfunContig ixf1 == ixfunContig ixf2) &&
  (NE.length (ixfunLMADs ixf1) == NE.length (ixfunLMADs ixf2)) &&
  all closeEnoughLMADs (NE.zip (ixfunLMADs ixf1) (ixfunLMADs ixf2))
  where
    closeEnoughLMADs :: (LMAD num, LMAD num) -> Bool
    closeEnoughLMADs (lmad1, lmad2) =
      length (lmadDims lmad1) == length (lmadDims lmad2) &&
      map ldPerm (lmadDims lmad1) ==
      map ldPerm (lmadDims lmad2)
