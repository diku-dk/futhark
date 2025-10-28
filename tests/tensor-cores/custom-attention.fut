-- ==
-- tags { no_cuda no_hip no_opencl }
-- entry: run16
-- only_intra compiled script input { (mk_input 128 16) } output { true }

-- ==
-- entry: run32
-- only_intra compiled script input { (mk_input 128 32) } output { true }

-- ==
-- entry: run64
-- only_intra compiled script input { (mk_input 128 64) } output { true }

-- ==
-- entry: run128
-- only_intra compiled script input { (mk_input 128 128) } output { true }

-- NOTE: Due to bug in memory block merging, these tests will not pass.

import "mmm-intra-helpers"

type real = f16

def matmul [d] (A: [d][d]real) (B: [d][d]real) : [d][d]real =
  map (\Arow ->
         map (\Bcol ->
                map2 (*) Arow Bcol
                |> reduce (+) 0.0)
             (transpose B))
      A

-- Note: Due to a compiler bug (described in the compiler)
-- this will give the wrong results
def oneIter [d] (K: [d][d]real) (V: [d][d]real) (Qi: [d][d]real) =
  let P_block = matmul_f16 Qi K
  in matmul P_block V

def flashAttention [m] [d]
                   (Q: [m][d][d]real)
                   (K: [d][d]real)
                   (V: [d][d]real) =
  map (oneIter K V) Q

#[inline]
def validate [m] [d]
             (expected: real)
             (Q: [m][d][d]real)
             (K: [d][d]real)
             (V: [d][d]real) =
  #[incremental_flattening(only_intra)]
  flashAttention Q K V
  |> flatten_3d
  |> map (== expected)
  |> and

entry mk_input (m: i64) (d: i64) : ([m][d][d]real, [d][d]real, [d][d]real) =
  let Q = replicate d 1.0 |> replicate d |> replicate m
  let K = replicate d 1.0 |> replicate d
  let V = replicate d 1.0 |> replicate d
  in (Q, K, V)

entry run16 [m] (Q: [m][16][16]real) (K: [16][16]real) (V: [16][16]real) =
  validate 256.0f16 Q K V

entry run32 [m] (Q: [m][32][32]real) (K: [32][32]real) (V: [32][32]real) =
  validate 1024.0f16 Q K V

entry run64 [m] (Q: [m][64][64]real) (K: [64][64]real) (V: [64][64]real) =
  validate 4096.0f16 Q K V

entry run128 [m] (Q: [m][128][128]real) (K: [128][128]real) (V: [128][128]real) =
  validate 16384.0f16 Q K V
