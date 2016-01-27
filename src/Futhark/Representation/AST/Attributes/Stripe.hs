module Futhark.Representation.AST.Attributes.Stripe
       ( stripeIndex
       , stripeIndexInverse
       , stripeToNumBlocks

       , stripeIndices
       , stripeIndicesInverse
       ) where

import Prelude hiding (quot, rem)

import Futhark.Util.IntegralExp

stripeIndex :: IntegralCond num => StripeParams num -> num -> num
stripeIndex params i =
  let j = flat i
      j' = (j `rem` n) + size_of_last
      j'' = j' + j' `quot` oneIfZero elems_per_short_block
      j''' = start_of_last + j''
      p = flat j'''
  in ifLessThan j n j p
  where (StripeParams
         n
         num_blocks
         elems_per_block
         elems_per_short_block
         start_of_last
         size_of_last) = params

        flat = flatIndex elems_per_block num_blocks

stripeIndexInverse :: IntegralCond num => StripeParams num -> num -> num
stripeIndexInverse params i =
  let j'''_inv = flat i
      j''_inv = j'''_inv - start_of_last
      j'_inv = (j''_inv * oneIfZero elems_per_short_block) `quotRoundingUp`
               (oneIfZero elems_per_short_block + 1)
      j_inv = j'_inv - size_of_last + n
      i_inv = flat j_inv
  in ifLessThan j'''_inv n j'''_inv i_inv
  where (StripeParams
         n
         num_blocks
         elems_per_block
         elems_per_short_block
         start_of_last
         size_of_last) = params

        flat = flatIndexInv elems_per_block num_blocks

flatIndex :: IntegralCond num => num -> num -> num -> num
flatIndex elems_per_block num_blocks i =
  i `quot` elems_per_block + (i `rem` elems_per_block) * num_blocks

-- | Compute @i@ such that @j == flatIndex elems_per_block
-- num_blocks i@.
flatIndexInv :: IntegralCond num => num -> num -> num -> num
flatIndexInv elems_per_block num_blocks =
  flatIndex num_blocks elems_per_block

data StripeParams num =
  StripeParams { stripeTotalElems :: num
               , stripeNumBlocks :: num
               , stripeElemsPerBlock :: num
               , stripeElemsPerShortBlock :: num
               , stripeStartOfLastBlock :: num
               , stripeSizeOfLastBlock :: num
               }
  deriving (Eq, Ord, Show)

stripeToNumBlocks :: IntegralCond num => num -> num -> StripeParams num
stripeToNumBlocks n num_blocks =
  StripeParams { stripeTotalElems =
                   n
               , stripeNumBlocks =
                   num_blocks
               , stripeElemsPerBlock =
                   elems_per_block
               , stripeElemsPerShortBlock =
                   elems_per_block - 1
               , stripeStartOfLastBlock =
                   start_of_last
               , stripeSizeOfLastBlock =
                   n - start_of_last
               }
  where elems_per_block = n `quotRoundingUp` num_blocks
        start_of_last = (n `quot` elems_per_block) * elems_per_block

stripeIndicesInverse :: Int -> Int -> [Int]
stripeIndicesInverse n num_blocks =
  map (wrappedValue . index . Wrapped) [0..n-1]
  where index = stripeIndexInverse (stripeToNumBlocks (Wrapped n) (Wrapped num_blocks))

stripeIndices :: Int -> Int -> [Int]
stripeIndices n num_blocks =
  map (wrappedValue . index . Wrapped) [0..n-1]
  where index = stripeIndex (stripeToNumBlocks (Wrapped n) (Wrapped num_blocks))
