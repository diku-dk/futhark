-- ==
-- tags { no_cuda no_hip no_opencl }
-- entry: mmm_small
-- compiled random input {[8][16][16][16]f16 [16][8][16][16]f16} auto output

-- ==
-- entry: mmm_large
-- compiled random input {[8][16][64][32]f16 [16][8][32][64]f16} auto output

import "mmm-intra-helpers"

def ne (m: i64) (n: i64) = (replicate (m * n) 0.0f32 |> unflatten)

def reduceOp [m] [n] (acc: *[m][n]f32) (elm: [m][n]f32) : [m][n]f32 =
  loop acc': *[m][n]f32 = (acc : *[m][n]f32)
  for i < m do
    acc' with [i, :] = map2 (+) elm[i] acc'[i]

def handleKBlocks [K] [m] [n] [k] (Arow: [K][m][k]f16) (Bcol: [K][k][n]f16) : [m][n]f32 =
  let acc_init = ne m n
  in loop (acc: *[m][n]f32) = acc_init
     for K_i < K do
       let C = matmul_f16_f32 Arow[K_i] Bcol[K_i]
       in reduceOp acc C

#[inline]
def mmm_intra [M] [K] [N] [m] [n] [k] (A: [M][K][m][k]f16) (B: [K][N][k][n]f16) : [M][N][m][n]f32 =
  #[incremental_flattening(only_inner)]
  map (\Arow ->
         #[incremental_flattening(only_intra)]
         map (\Bcol ->
                handleKBlocks Arow Bcol)
             (transpose B))
      A

entry mmm_small (A: [8][16][16][16]f16) (B: [16][8][16][16]f16) =
  mmm_intra A B

entry mmm_large (A: [8][16][64][32]f16) (B: [16][8][32][64]f16) =
  mmm_intra A B
