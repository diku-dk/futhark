-- ==
-- compiled random input {[1024][64][64]f16 [1024][256][64][64]f16}
-- nobench compiled random input {[16][64][64]f16 [16][12][64][64]f16} auto output
import "mmm-intra-helpers"

let k = 64i64
let m = 64i64
let n = 64i64


def seq_acc (acc: *[m][n]f32) (C: *[m][n]f32) =
  let acc_flat = flatten acc
  let Cflat = flatten C
  let thrd_work = m * n / 32 in
  let res = tabulate 32 (\thrd_idx ->
                           let start = thrd_idx * thrd_work in
                           loop acc' = replicate thrd_work 0.0f32
                           for i < thrd_work do
                             acc' with [i] = acc_flat[start + i] + Cflat[start + i]
                        )
  let res_flat = sized (m * n) (flatten res)
  in unflatten res_flat

def seq_acc2 (acc: *[m][n]f32) (C: *[m][n]f32) =
  let thrd_work = m * n / 32 in
  loop acc' = acc for j < thrd_work do
  let js_per_row = n / 32 in
  let row = j / js_per_row
  let col = j % js_per_row
  let col_offset = col * 32
  in acc' with [row, col_offset:col_offset + 32] =
    tabulate 32 (\k -> acc'[row, col_offset + k] + C[row, col_offset + k])

let seq_acc3 (acc: *[m][n]f32) (C: [m][n]f32) : *[m][n]f32 =
  loop acc': *[m][n]f32 = (acc : *[m][n]f32) for i < m do
      acc' with [i, :] = map2 (+) C[i] acc'[i]

def attention_like [q] (A: [m][k]f16) (B: [q][k][n]f16) : [m][n]f32 =
  -- Copy to shared
  let A' = if q > 1
           then copy A
           else replicate (m * k) 0.0f16 |> unflatten

  let acc_init : *[m][n]f32 = replicate (m * n) 0.0f32 |> unflatten in
  loop (acc : *[m][n]f32) = (acc_init: *[m][n]f32) for i < q do
    let B' = B[i]
    let C : *[m][n]f32 = matmul A' B' in
    copy C
    -- seq_acc3 acc C
    -- seq_acc acc C
    -- if true then C else acc
    -- map2 (map2 (+)) acc C

def main [q][p] (A: [p][m][k]f16) (B: [p][q][k][n]f16) =
  #[incremental_flattening(only_intra)]map2 attention_like A B
--  map2 attention_like A B
