-- | An index function represents a mapping from an array index space
-- to a flat byte offset.   This implements a representation for the
-- index function based on linear-memory accessor descriptors, see
-- Zhu, Hoeflinger and David work.   Our specific representation is:
-- LMAD = \overline{s,r,n}^k + o, where `o` is the offset, and `s_j`,
-- `r_j`, and `n_j` are the stride, the rotate factor and the number
-- of elements on dimension j. Dimensions are ordered in row major fashion.
-- By definition, the LMAD above denotes the set of points:
-- \{ o + \Sigma_{j=0}^{k} ((i_j+r_j) `mod` n_j)*s_j,
--    \forall i_j such that 0<=i_j<n_j, j=1..k \}
--
module Futhark.Representation.ExplicitMemory.Lmad
       (
         IxFun(..)
       , index
       , iota
--       , offsetIndex
--       , strideIndex
       , permute
       , rotate
       , reshape
       , slice
--       , base
--       , rebase
       , repeat
       , isContiguous
       , shape
       , rank
       , getMonotonicity
--       , linearWithOffset
--       , rearrangeWithOffset
--       , isDirect
--       , substituteInIxFun
       )
       where

--import Control.Applicative
--import Data.Maybe
import Data.List as L hiding (repeat)
--import Data.Monoid
--import Data.List hiding (repeat)
import Control.Monad.Identity
--import Control.Monad.Writer

import Prelude hiding (mod, repeat)

--import qualified Data.Map.Strict as M

--import Futhark.Transform.Substitute
--import Futhark.Transform.Rename

import Futhark.Representation.AST.Syntax
  (ShapeChange, DimChange(..), DimIndex(..), Slice, unitSlice) --VName, sliceDims
--import Futhark.Representation.AST.Attributes.Names
import Futhark.Representation.AST.Attributes.Reshape
--import Futhark.Representation.AST.Attributes.Rearrange
--import Futhark.Representation.AST.Pretty ()
import Futhark.Util.IntegralExp
import Futhark.Util.Pretty
--import Futhark.Analysis.PrimExp.Convert
--import Futhark.Util

--import Debug.Trace

type Shape num   = [num]
type Indices num = [num]
type Permutation = [Int]

-- | TODO: should only be: Inc | Dec | Unknown
--         because together with the contiguosness
--         this is enough information
data DimInfo = Inc | Dec | Unknown
               -- ^ monotonously increasing, decreasing or unknwon
             deriving (Show,Eq)

-- | LMAD's representation consists of a permutation,
--   a general offset, and, for each dimension a stride,
--   rotate factor, number of elements, permutation, and
--   ``fullness'' and unit-stride info for each dimension.
--   Note that the permutation is not strictly necessary
--   in that the permutation can be performed directly
--   on Lmad dimensions, but then it is difficult to
--   extract the permutation back from an Lmad.
data Lmad num = Lmad num [(num,num,num,Int,DimInfo)]

-- | LMAD algebra is closed under composition w.r.t.
--     operators such as permute, repeat, index and slice.
--     However, other operations, such as reshape, cannot be
--     always represented inside the LMAD algebra.
--   It follows that the general representation of an index
--     function is a list of LMADS, in which each following
--     LMAD in the list implicitly corresponds to an irregular
--     reshaping operation.
--   However, we expect that the common case is when the index
--     function is one LMAD -- we call this the `Nice` representation.
--   Finally, the list of LMADs is tupled with the shape of the
--     original array, and with contiguous info, i.e., if we instantiate
--     all the points of the current index function, do we get a
--     contiguous memory interval?
data IxFun num = IxFun [Lmad num] (Shape num) Bool
               -- ^ list of Lmads * shape of original array * contiguous-in-mem

-- | whether an index function has contiguous memory support
isContiguous :: (Eq num, IntegralExp num) => IxFun num -> Bool
isContiguous (IxFun _ _ cg) = cg

-- | Shape of an Lmad
shape0 :: (Eq num, IntegralExp num) => Lmad num -> Shape num
shape0 (lmad@(Lmad _ srns)) =
  map (\(_,_,z,_,_)->z) $ permuteInv (getPermutation lmad) srns

-- | Shape of an index function
shape :: (Eq num, IntegralExp num) => IxFun num -> Shape num
shape (IxFun [] _ _) = error "shape: empty index function"
shape (IxFun (lmad:_) _ _) = shape0 lmad

-- | Computing the flat memory index for a complete set `inds`
--     of array indices and a certain element size `elem_size`.
index :: (Pretty num, IntegralExp num, Eq num) =>
          IxFun num -> Indices num -> num -> num
index (IxFun [] _ _) _ _ = error "index: empty index function"
index (IxFun [lmad] _ _) iis elm_size = index0 lmad iis elm_size
index (IxFun (lmad1:lmad2:lmads) oshp c) iis elm_size =
  let i_flat   = index0 lmad1 iis 1
      new_inds = unflattenIndex (shape0 lmad2) i_flat
  in  index (IxFun (lmad2:lmads) oshp c) new_inds elm_size

-- | Helper for index: computing the flat index of an Lmad.
index0 :: (Pretty num, Eq num, IntegralExp num) =>
          Lmad num -> Indices num -> num -> num
index0 lmad@(Lmad tau srnps) inds elm_size =
  let srns  = permuteFwd (getPermutation lmad) $
                         map (\(s,r,n,_,_) -> (s,r,n)) srnps
      prods = sum $ zipWith flatOneDim srns inds
  in  (tau + prods) * elm_size


-- | iota
iota :: (Pretty num, IntegralExp num) => Shape num -> IxFun num
iota ns = IxFun [makeRotIota Inc 0 $ zip rs ns] ns True
  where rs = replicate (length ns) 0

-- | permute dimensions
permute :: IntegralExp num =>
           IxFun num -> Permutation -> IxFun num
permute (IxFun [] _ _) _ = error "permute: empty index function"
permute (IxFun (lmad:lmads) oshp cg) ps =
  let perm = map (\p -> ps !! p) $ getPermutation lmad
  in  IxFun (setPermutation perm lmad : lmads) oshp cg

-- | repeating dimensions
repeat :: (Eq num, IntegralExp num) =>
          IxFun num -> [Shape num] -> Shape num -> IxFun num
repeat (IxFun [] _ _) _ _ = error "repeat: empty index function"
repeat (IxFun (lmad@(Lmad tau srnps) : lmads) oshp cg) shps shp =
  let perm = getPermutation lmad
      -- inverse permute the shapes and update the permutation!
      lens = map (\s -> 1 + length s) shps
      (shps', lens') = unzip $ permuteInv perm $ zip shps lens
      scn = drop 1 $ scanl (+) 0 lens'
      perm' = concatMap (\(p,l) -> map (\i-> (scn!!p)-l+i) [0..l-1])
                        $ zip perm lens
      tmp = length perm'
      perm'' = perm' ++ [tmp..tmp-1+length shp]

      srnps' = concatMap (\(shp_k, srnp)->
                              map fakeDim shp_k ++ [srnp]
                         ) $ zip shps' srnps
      lmad' = setPermutation perm'' $ Lmad tau (srnps' ++ map fakeDim shp)
  in  IxFun (lmad' : lmads) oshp cg
  where fakeDim x = (0,0,x,0,Unknown)

-- | Rotating an index function:
rotate :: (Eq num, IntegralExp num) =>
          IxFun num -> Indices num -> IxFun num
rotate  (IxFun [] _ _) _ = error "rotate: empty index function"
rotate  (IxFun (lmad@(Lmad off srnps) : lmads) oshp cg) offs =
  let srnps' = zipWith (\(s,r,n,p,f) o ->
                          if s == 0 then (0,0,n,p,Unknown)
                          else (s,r+o,n,p,f)
                       ) srnps $ permuteInv (getPermutation lmad) offs
  in  IxFun (Lmad off srnps':lmads) oshp cg


-- | Slicing an index function.
slice :: (Pretty num, Eq num, IntegralExp num) =>
         IxFun num -> Slice num -> IxFun num
slice (IxFun [] _ _) _ = error "slice: empty index function"
slice _ [] = error "slice: empty slice ???"
slice ixfn dim_slices
  -- Avoid identity slicing.
  | dim_slices == map (unitSlice 0) (shape ixfn) = ixfn
slice (IxFun (lmad@(Lmad _ srnpfs):lmads) oshp cg) is =
  let perm= getPermutation lmad
      is' = permuteInv perm is
      contig = cg && preservesContiguous lmad is'
  in  if  harmlessRotation lmad is'
      then let lmad' = foldl sliceOne (Lmad (getOffset lmad) [])
                         $ zip is' srnpfs
               -- need to remove the fixed dims from the permutation
               perm' = updatePerm perm $ map fst $ filter isFixedDim $
                                  zip [0..length is' - 1] is'
           in  IxFun (setPermutation perm' lmad':lmads) oshp contig
      else -- falls outside LMAD formula, hence append a new LMAD
           case slice (iota (shape0 lmad)) is of
             IxFun [lmad'] _ _ -> IxFun (lmad':lmad:lmads) oshp contig
             _ -> error "slice: reached impossible case!"
  where isFixedDim (_,DimFix{}) = True
        isFixedDim _            = False

        updatePerm ps inds = foldl (\acc p -> acc ++ decrease p) [] ps
          where decrease p =
                  let d = foldl (\n i -> if i == p then (-1)
                                         else if i > p
                                              then n
                                              else if n /= (-1) then n+1
                                                   else n
                                ) 0 inds
                  in  if d == (-1) then [] else [p-d]

        harmlessRotation0 :: (Eq num, IntegralExp num) =>
                             (num,num,num,Int,DimInfo) -> DimIndex num -> Bool
        harmlessRotation0 _ (DimFix _)   = True
        harmlessRotation0 (0,_,_,_,_) _  = True
        harmlessRotation0 (_,0,_,_,_) _  = True
        harmlessRotation0 (_,_,n,_,_) dslc
            | dslc == DimSlice (n-1) n (-1) ||
              dslc == unitSlice 0 n      = True
        harmlessRotation0 _ _            = False

        harmlessRotation :: (Eq num, IntegralExp num) =>
                             Lmad num -> Slice num -> Bool
        harmlessRotation (Lmad _ srnps) iss =
            and $ zipWith harmlessRotation0 srnps iss

        -- | TODO: what happens to r on a negative-stride slice; is there a such case?
        sliceOne :: (Eq num, IntegralExp num) =>
                    Lmad num -> (DimIndex num, (num,num,num,Int,DimInfo)) -> Lmad num
        sliceOne (Lmad tau srns) (DimFix i, (s,r,n,_,_)) =
            Lmad (tau + flatOneDim (s,r,n) i) srns
        sliceOne (Lmad tau srns) (DimSlice _ ne _, (0,_,_,p,_)) =
            Lmad tau (srns ++ [(0,0,ne,p,Unknown)])
        sliceOne (Lmad tau srns) (dmind, srn@(_,_,n,_,_))
            | dmind == unitSlice 0 n = Lmad tau (srns ++ [srn])
        sliceOne (Lmad tau srns) (dmind, (s,r,n,p,f))
            | dmind == DimSlice (n-1) n (-1) =
              let r' = if r == 0 then 0 else n-r
              in  Lmad tau' (srns ++ [(s*(-1),r',n,p, invertInfo f)])
              where tau' = tau + flatOneDim (s,0,n) (n-1)
        sliceOne (Lmad tau srns) (DimSlice b ne 0, (s,r,n,p,_)) =
            Lmad (tau + flatOneDim (s,r,n) b) (srns ++ [(0,0,ne,p,Unknown)])
        sliceOne (Lmad tau srns) (DimSlice bs ns ss, (s,0,_,p,f)) =
            let f' = case sgn ss of
                       Just 1    -> f
                       Just (-1) -> invertInfo f
                       _         -> Unknown
            in  Lmad (tau + s*bs) (srns ++ [(ss*s,0,ns,p,f')])
        sliceOne _ _ = error "slice: reached impossible case!"

        normIndex :: (Eq num, IntegralExp num) =>
                     DimIndex num -> DimIndex num
        normIndex (DimSlice b 1 _) = DimFix b
        normIndex (DimSlice b _ 0) = DimFix b
        normIndex d = d

        preservesContiguous :: (Eq num, IntegralExp num) =>
                               Lmad num -> Slice num -> Bool
        preservesContiguous (Lmad _ srnps) slc =
          -- remove from the slice the Lmad dimensions who have stride 0.
          -- If the Lmad was contiguous in mem, then these dims will not
          -- influence the contiguousness of the result.
          -- Also normalize the input slice, i.e., 0-stride and size-1
          -- slices are rewritten as DimFixed.
          let (srnps', slc') = unzip $
                filter (\((s,_,_,_,_),_) -> s /= 0) $
                       zip srnps $ map normIndex slc
              -- Check that:
              -- 1. a clean split point exists between Fixed and Sliced dims
              -- 2. the outermost sliced dim has +/- 1 stride AND is unrottated or full.
              -- 3. the rest of inner sliced dims are full.
              (_, success) =
                foldl (\(found,res) (slcdim, (_,r,n,_,_)) ->
                        case (slcdim, found) of
                          (DimFix{},   True ) -> (found, False)
                          (DimFix{},   False) -> (found, res)
                          (DimSlice _ ne ds, False) -> -- outermost sliced dim: +/-1 stride
                            let res' = (r == 0 || n == ne) && (ds == 1 || ds == (-1))
                            in  (True, res && res')
                          (DimSlice _ ne ds, True) ->  -- inner sliced dim: needs to be full
                            let res' = (n == ne) && (ds == 1 || ds == (-1))
                            in  (found, res && res')
                      ) (False,True) $ zip slc' srnps'
          in  success

-- | Reshaping an index function.
--   There are four conditions that all must hold for the result
--   of a reshape operation to remain into the one-Lmad domain:
--   (1) the permutation of the underlying Lmad must leave unchanged
--       the Lmad dimensions that were *not* reshape coercions.
--   (2) the repetition of dimensions of the underlying Lmad must
--       refer only to the coerced-dimensions of the reshape operation.
--   (3) similarly, the rotated dimensions must refer only to
--       dimensions that are coerced by the reshape operation.
--   (4) finally, the underlying memory is contiguous (and monotonous)
--
--   If any of this conditions does not hold then the reshape operation
--   will conservatively add a new Lmad to the list, leading to a
--   representation that provides less opportunities for further analysis.
--
--   Actually there are some special cases that need to be treated,
--   for example if everything is a coercion, then it should succeed
--   no matter what.
reshape :: (Pretty num, Eq num, IntegralExp num) =>
           IxFun num -> ShapeChange num -> IxFun num
reshape (IxFun [] _ _) _ =
  error "reshape: empty index function"

reshape ixfn@(IxFun (lmad@(Lmad tau srnps):lmads) oshp cg) newshape
  | perm <- getPermutation lmad,
    --- WRONG: newshapeorig probably has less dimensions!!!
    ---        instead of this, please permuteFwd the srnps
    ---        and restart from there
    Just (head_coercions, reshapes, tail_coercions) <-
      splitCoercions newshape,
    hd_len <- length head_coercions,
    num_coercions <- hd_len + length tail_coercions,
    srnps' <- permuteFwd perm srnps,
    mid_srnps <- take (length srnps - num_coercions) $
                      drop hd_len srnps',
    num_rshps <- length reshapes,
    num_rshps == 0 || (num_rshps == 1 && length mid_srnps == 1),
    srnps'' <- map snd $ L.sortBy sortGT $
               zipWith (\(s,r,_,p,f) n -> (p,(s,r,n,p,f)))
                       srnps' $ newDims newshape
    = IxFun (Lmad tau srnps'':lmads) oshp cg

  | perm <- getPermutation lmad,
    Just (head_coercions, reshapes, tail_coercions) <-
      splitCoercions newshape,
    hd_len <- length head_coercions,
    num_coercions <- hd_len + length tail_coercions,
    srnps_perm <- permuteFwd perm srnps,
    mid_srnps <- take (length srnps - num_coercions) $
                      drop hd_len srnps_perm,
    -- checking conditions (2) and (3)
    all (\ (s,r,_,_,_) -> s /= 0 && r == 0) mid_srnps,
    -- checking condition (1)
    consecutive hd_len $ map (\(_,_,_,p,_)->p) mid_srnps,
    -- checking condition (4)
    info <- getMonotonicityRots True ixfn,
    cg && (info == Inc || info == Dec),
    -- make new permutation
    rsh_len <- length reshapes,
    diff <- length newshape - length srnps,
    iota_shape <- [0..length newshape-1],
    perm' <- map (\i -> let ind = if i < hd_len
                                  then i else i - diff
                        in  if (i>=hd_len) && (i < hd_len+rsh_len)
                            then i -- already checked mid_srnps not affected
                            else let (_,_,_,p,_) = srnps !! ind
                                 in  if p < hd_len
                                     then p else p + diff
                 ) iota_shape,
    -- split the dimensions
    (suport_inds, repeat_inds) <-
      foldl (\(sup,rpt) (i,shpdim,ip) ->
              case (i < hd_len, i >= hd_len+rsh_len, shpdim) of
                (True,  _, DimCoercion n) ->
                  case srnps_perm !! i of
                    (0,_,_,_,_) -> ( sup, (ip,n) : rpt )
                    (_,r,_,_,_) -> ( (ip,(r,n)) : sup, rpt )
                (_,  True, DimCoercion n) ->
                  case srnps_perm !! (i-diff) of
                    (0,_,_,_,_) -> ( sup, (ip,n) : rpt )
                    (_,r,_,_,_) -> ( (ip,(r,n)) : sup, rpt )
                (False, False, _) ->
                    ( (ip, (0, newDim shpdim)) : sup, rpt )
                    -- ^ already checked that the reshaped
                    --   dims cannot be repeats or rotates
                _ -> error "reshape: reached impossible case!"
            ) ([],[]) $ reverse $ zip3 iota_shape newshape perm',

    (sup_inds, support) <- unzip $ L.sortBy sortGT suport_inds,
    (rpt_inds, repeats) <- unzip repeat_inds,
    Lmad tau' srnps_sup <- makeRotIota info tau support,
    repeats' <- map (\n -> (0,0,n,0,Unknown)) repeats,
    srnps'   <- map snd $ L.sortBy sortGT $
                zip sup_inds srnps_sup ++ zip rpt_inds repeats'
    = IxFun (setPermutation perm' (Lmad tau' srnps') : lmads) oshp cg
  where splitCoercions newshape' = do
          let (head_coercions, newshape'') = span isCoercion newshape'
          let (reshapes, tail_coercions) = break isCoercion newshape''
          guard (all isCoercion tail_coercions)
          return (head_coercions, reshapes, tail_coercions)

        isCoercion DimCoercion{} = True
        isCoercion _ = False

        consecutive _ [] = True
        consecutive i [p]= i == p
        consecutive i ps = and $ zipWith (==) ps [i, i+1..]

reshape (IxFun lmads oshp cg) newshape =
  let new_dims = newDims newshape
  in case iota new_dims of
       IxFun [lmad] _ _ -> IxFun (lmad : lmads) oshp cg
       _ -> error "reshape: impossible case reached"


rank :: IntegralExp num =>
        IxFun num -> Int
rank (IxFun [] _ _) = error "rank: empty index function"
rank (IxFun (Lmad _ sss : _) _ _) = length sss

getMonotonicity :: (Eq num, IntegralExp num) => IxFun num -> DimInfo
getMonotonicity = getMonotonicityRots False

getMonotonicityRots :: (Eq num, IntegralExp num) => Bool -> IxFun num -> DimInfo
getMonotonicityRots _ (IxFun [] _ _) =
  error "getMonotonicityRots: empty index function"
getMonotonicityRots ignore_rots (IxFun (lmad:lmads) _ _) =
  let mon1 = getLmadMonotonicity ignore_rots lmad
  in  if all (==mon1) $ map (getLmadMonotonicity ignore_rots) lmads
      then mon1 else Unknown

getLmadMonotonicity :: (Eq num, IntegralExp num) => Bool -> Lmad num -> DimInfo
getLmadMonotonicity ignore_rots (Lmad _ dims)
  | all (isMonDim ignore_rots Inc) dims = Inc
  | all (isMonDim ignore_rots Dec) dims = Dec
  | otherwise                           = Unknown

isMonDim :: (Eq num, IntegralExp num) => Bool -> DimInfo ->
            (num, num, num, Int, DimInfo) -> Bool
isMonDim ignore_rots mon (s,r,_,_,info) =
  s == 0 || ((ignore_rots || r == 0) && mon == info)

------------------------------------------
--- COSMIN is here with the re-writing ---
------------------------------------------

{--
offsetIndex :: (Eq num, IntegralExp num) =>
               Lmads num -> num -> Lmads num
offsetIndex ixfun i | i == 0 = ixfun
offsetIndex ixfun i =
  case shape ixfun of
    d:ds -> slice ixfun (DimSlice i (d-i) 1 : map (unitSlice 0) ds)
    []   -> error "offsetIndex: underlying index function has rank zero"

strideIndex :: (Eq num, IntegralExp num) =>
               Lmads num -> num -> Lmads num
strideIndex ixfun s =
  case shape ixfun of
    d:ds -> slice ixfun (DimSlice 0 d s : map (unitSlice 0) ds)
    []   -> error "offsetIndex: underlying index function has rank zero"
--}

{--
rearrangeWithOffset :: (Eq num, IntegralExp num) =>
                       Lmads num -> Lmads num -> num -> Maybe (num, num, Permutation)
rearrangeWithOffset [Lmad tau1 ss1] [Lmad tau2 ss2] elm_size =
  let dims1 = map getDimLogicalSize ss1
      dims2 = map getDimLogicalSize ss2
      ok1   = foldl (&&) True $ zipWith (==) dims1 dims2

rearrangeWithOffset _ _ _ = Nothing
rearrangeWithOffset (Reshape ixfun _) element_size =
  rearrangeWithOffset ixfun element_size
rearrangeWithOffset (Permute ixfun perm) element_size = do
  offset <- linearWithOffset ixfun element_size
  return (offset, zip perm $ rearrangeShape perm $ shape ixfun)
rearrangeWithOffset _ _ =
  Nothing
--}

------------------------
--- Helper functions ---
------------------------

invertInfo :: DimInfo -> DimInfo
invertInfo Inc = Dec
invertInfo Dec = Inc
invertInfo Unknown = Unknown

getOffset :: Lmad num -> num
getOffset (Lmad tau _) = tau

getPermutation :: Lmad num -> Permutation
getPermutation (Lmad _ srns) = map (\(_,_,_,p,_) -> p) srns

setPermutation :: Permutation -> Lmad num -> Lmad num
setPermutation perm (Lmad tau srnps) =
  Lmad tau $ zipWith (\(s,r,n,_,i) p -> (s,r,n,p,i)) srnps perm

permuteFwd :: Permutation -> [a] -> [a]
permuteFwd [] _ = []
permuteFwd (p:ps) ds = (ds !! p) : permuteFwd ps ds

permuteInv :: Permutation -> [a] -> [a]
permuteInv ps elems = map snd $ L.sortBy sortGT $ zip ps elems

sortGT :: Ord a => (a, b1) -> (a, b2) -> Ordering
sortGT (a1, _) (a2, _)
  | a1 > a2   = GT
  | a1 < a2   = LT
  | otherwise = GT

flatOneDim ::  (Eq num, IntegralExp num) =>
               (num, num, num) -> num -> num
flatOneDim (s,r,n) i
  | s == 0 = 0
  | r == 0 = i*s
  | otherwise = ((i+r) `mod` n) * s

makeRotIota :: (Pretty num, IntegralExp num) =>
               DimInfo -> num -> [(num,num)] -> Lmad num
makeRotIota info tau support
  | info == Inc || info == Dec =
    let rk = length support
        (rs,ns) = unzip support
        ss0= L.reverse $ take rk $ scanl (*) 1 $ L.reverse ns
        ss = if info == Inc then ss0
             else map (*(-1)) ss0
        ps = map fromIntegral [0..rk-1]
        fi = replicate rk info
    in  Lmad tau $ zip5 ss rs ns ps fi
  | otherwise = error "makeRotIota requires Inc or Dec!"

--------------------------------
--- Instances Implementation ---
--------------------------------

instance Pretty DimInfo where
  ppr Inc      = text "I"
  ppr Dec      = text "D"
  ppr Unknown  = text "U"

instance Pretty num => Pretty (Lmad num) where
  ppr (Lmad tau srnps) =
    let (ss, rs, ns, ps, fs) = unzip5 srnps
    in text " | " <> ppr tau <>
        text " + " <> brackets (commasep $ map ppr ss) <>
        text "v" <> brackets (commasep $ map ppr rs) <>
        text "v" <> brackets (commasep $ map ppr ns) <>
        text "v" <> brackets (commasep $ map ppr ps) <>
        text "v" <> brackets (commasep $ map ppr fs) <>
        text " | "

instance Pretty num => Pretty (IxFun num) where
  ppr (IxFun lmads orgshp cg) =
    text "Shape: " <> braces (commasep $ map ppr orgshp) <>
    text " LMADS: " <> braces (stack $ map ppr lmads)    <>
    text " CONTIG: "<> text (show cg)
{--
    text "Direct" <> parens (commasep $ map ppr dims)
  ppr (Permute fun perm) = ppr fun <> ppr perm
  ppr (Rotate fun offsets) = ppr fun <> brackets (commasep $ map ((text "+" <>) . ppr) offsets)
  ppr (Index fun is) = ppr fun <> brackets (commasep $ map ppr is)
  ppr (Reshape fun oldshape) =
    ppr fun <> text "->reshape" <>
    parens (commasep (map ppr oldshape))
  ppr (Repeat fun outer_shapes inner_shape) =
    ppr fun <> text "->repeat" <> parens (commasep (map ppr $ outer_shapes++ [inner_shape]))
--}
