-- ==
-- compiled random input { [2][2][32][32]f32 [32][32]f32 } auto output

def dotprod [n] (a: [n]f32) (b: [n]f32) : f32 =
  map2 (*) a b
  |> reduce (+) 0

def lud_perimeter_lower [b] [m] (diag: [b][b]f32) (mat: [m][b][b]f32) : *[m][b][b]f32 =
  map (\blk ->
         #[incremental_flattening(only_intra)]
         map (\row0 ->
                -- Lower
                #[unsafe]
                loop row = copy row0
                for j < b do
                  let sum =
                    loop sum = 0.0f32
                    for k < j do
                      sum + diag[k, j] * row[k]
                  let row[j] = (row[j] - sum) / diag[j, j]
                  in row)
             blk)
      mat

def main [num_blocks] [b] (matb: *[num_blocks][num_blocks][b][b]f32) (diag: [b][b]f32) =
  let step = 0
  let col_slice = matb[step + 1:num_blocks, step]
  let lft_per_irreg = lud_perimeter_lower diag col_slice
  in lft_per_irreg
