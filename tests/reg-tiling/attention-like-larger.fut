-- ==
-- compiled random input {[1024][128][64]f16 [1024][256][64][128]f16}
-- nobench compiled random input {[16][128][64]f16 [16][12][64][128]f16} auto output

-- compiled script input { (mk_attention_like_input 512 256 128 128 64) }

import "mmm-intra-helpers"

let k = 64i64
let m = 128i64
let n = 128i64

def attention_like [q] (A: [m][k]f16) (B: [q][k][n]f16) : [m][n]f32 =
  -- Copy to shared
  let A' = if q > 1
           then copy A
           else replicate (m * k) 0.0f16 |> unflatten
  let acc_init : *[m][n]f32 = replicate (m * n) 0.0f32 |> unflatten in
  loop (acc : *[m][n]f32) = (acc_init: *[m][n]f32) for i < q do
  let B' = B[i]
  let C : *[m][n]f32 = matmul A' B' in
  C
  -- map2 (map2 (+)) acc C


def main [q][p] (A: [p][m][k]f16) (B: [p][q][k][n]f16) =
  map2 attention_like A B

  -- Attempts of sequentialisation

  -- let C_flat = flatten C
  -- let (acc_flat : [m * n]f32) = flatten acc
  -- let work = m * n / 32 in
  -- tabulate 32 (\i ->
  --                let start = i * work
  --                let end = (i+1) * work
  --                let acc_slice : [end-start]f32 = acc_flat[start : end]
  --                let C_slice : [end-start]f32 = C_flat[start : end]
  --                let res =
  --                  loop (acc' : *[end-start]f32) = (acc_slice : *[end-start]f32)
  --                  for k < work do
  --                    acc' with [k] = C_slice[k] + acc_slice[k]
  --                in res :> [m * n / 32]f32
  --             ) :> [m][n]f32

-- map2 (map2 (+)) C acc
-- loop C': *[m][n]f32 = (C : *[m][n]f32) for i < m do
-- C' with [i, :] = map2 (+) C'[i] acc[i]

-- loop C': *[m][n]f32 = (C : *[m][n]f32) for i < m do
-- loop C'' = C' for j < 2 do
-- let start = j * (k / 2)
-- let end = (j+1) * (k / 2) in
-- C'' with [i, start : end] = map2 (+) C''[i, start:end] acc[i, start:end]


  -- let res : [m][n]f32 =
  --   loop acc' = acc for i < m do
  --     let C_row = C[i]
  --     let transformed_acc_row =
  --       loop acc_row = copy acc'[i] for j < 2 do
  --         let start = (n / 2) * j
  --         let end = (n / 2 + 1) * j in
  --         acc_row with [start:end] = map2 (+) acc_row[start:end] C_row[start:end]
  --     in
  --     acc' with [i, :] = transformed_acc_row
  -- in res


