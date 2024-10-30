-- ==
-- nobench compiled random input {[16][32][32]f16 [16][16][32][32]f16} auto output
-- compiled random input {[1024][32][32]f16 [1024][64][32][32]f16}
import "mmm-intra-helpers"

let k = 32i64
let m = 32i64
let n = 32i64


def attention_like [q] (A: [m][k]f16) (B: [q][k][n]f16) : [m][n]f32 =
  -- Copy to shared
  let A' = if q > 11
           then copy A
           else replicate (m * k) 0.0f16 |> unflatten                                            
  let acc_init : *[m][n]f32 = replicate (m * n) 0.0f32 |> unflatten in  
  loop (acc : *[m][n]f32) = (acc_init: *[m][n]f32) for i < q do  
  let B' = B[i]
  let C : *[m][n]f32 = matmul A' B' in
  -- map2 (map2 (+)) C acc
  loop C': *[m][n]f32 = (C : *[m][n]f32) for i < m do    
    C' with [i, :] = map2 (+) C'[i] acc[i]

  

def main [q][p] (A: [p][m][k]f16) (B: [p][q][k][n]f16) =
  #[incremental_flattening(only_intra)]map2 attention_like A B
