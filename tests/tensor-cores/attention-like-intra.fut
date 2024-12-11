-- ==
-- entry: test16
-- compiled nobench random input {[32][16][16]f16 [32][4][16][16]f16} auto output

-- ==
-- entry: test32
-- compiled nobench random input {[32][32][32]f16 [32][4][32][32]f16} auto output

-- ==
-- entry: test64
-- compiled nobench random input {[32][64][64]f16 [32][4][64][64]f16} auto output

-- ==
-- entry: test128
-- compiled nobench large random input {[32][128][128]f16 [32][4][128][128]f16} auto output

import "mmm-intra-helpers"

let seq_acc [m][n] (acc: *[m][n]f32) (C: *[m][n]f32) =
  loop acc': *[m][n]f32 = (acc : *[m][n]f32) for i < m do
      acc' with [i, :] = map2 (+) C[i] acc'[i]

def attention_like [q][m][n][k] (A: [m][k]f16) (B: [q][k][n]f16) : [m][n]f32 =
  -- Copy to shared
  let A' = if q > 1
           then copy A
           else replicate (m * k) 0.0f16 |> unflatten

  let acc_init : *[m][n]f32 = replicate (m * n) 0.0f32 |> unflatten in
  loop acc = (acc_init: *[m][n]f32) for i < q do
    let B' = B[i]
    let C : *[m][n]f32 = matmul_f16_f32 A' B'
    in seq_acc acc C

entry test16 [q][p] (A: [p][16][16]f16) (B: [p][q][16][16]f16) =
  #[incremental_flattening(only_intra)]map2 attention_like A B

entry test32 [q][p] (A: [p][32][32]f16) (B: [p][q][32][32]f16) =
  #[incremental_flattening(only_intra)]map2 attention_like A B    

entry test64 [q][p] (A: [p][64][64]f16) (B: [p][q][64][64]f16) =
  #[incremental_flattening(only_intra)]map2 attention_like A B

entry test128 [q][p] (A: [p][128][128]f16) (B: [p][q][128][128]f16) =
  #[incremental_flattening(only_intra)]map2 attention_like A B
