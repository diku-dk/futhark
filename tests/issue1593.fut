-- ==
-- structure gpu { Replicate 0 }

def lud_perimeter_upper [m] [b] (diag: [b][b]f32, a0s: [m][b][b]f32) : *[m][b][b]f32 =
  let a1s = map (\(x: [b][b]f32) : [b][b]f32 -> transpose (x)) a0s
  let a2s =
    map (\a1 : [b][b]f32 ->
           map (\row0 : [b]f32 ->
                  -- Upper
                  loop row = copy row0
                  for i < b do
                    let sum = (loop sum = 0.0f32 for k < i do sum + diag[i, k] * row[k])
                    let row[i] = row[i] - sum
                    in row)
               a1)
        a1s
  in map (\x : [b][b]f32 -> transpose (x)) a2s

def main [num_blocks] (matb: *[num_blocks][num_blocks][32][32]f32) : *[num_blocks][num_blocks][32][32]f32 =
  #[unsafe]
  let matb =
    loop (matb) for step < num_blocks - 1 do
      -- 1. compute the current diagonal block
      let diag = matb[step, step]
      -- 2. compute the top  perimeter
      let row_slice = matb[step, step + 1:num_blocks]
      let top_per_irreg = lud_perimeter_upper (diag, row_slice)
      -- 5. update matrix in place
      let matb[step, step + 1:num_blocks] = top_per_irreg
      in matb
  in matb
