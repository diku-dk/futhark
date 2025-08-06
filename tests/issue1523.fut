def dotprod [n] (a: [n]f32) (b: [n]f32) : f32 =
  map2 (*) a b
  |> reduce (+) 0

def lud_diagonal [b] (a: [b][b]f32) : *[b][b]f32 =
  let mat = copy a
  in #[unsafe]
     loop (mat: *[b][b]f32) for i < b - 1 do
       let col =
         map (\j ->
                if j > i
                then (mat[j, i] - (dotprod mat[j, :i] mat[:i, i])) / mat[i, i]
                else mat[j, i])
             (iota b)
       let mat[:, i] = col
       let row =
         map (\j ->
                if j > i
                then mat[i + 1, j] - (dotprod mat[:i + 1, j] mat[i + 1, :i + 1])
                else mat[i + 1, j])
             (iota b)
       let mat[i + 1] = row
       in mat

def lud_perimeter_upper [m] [b] (diag: [b][b]f32) (a0s: [m][b][b]f32) : *[m][b][b]f32 =
  let a1s = map (\(x: [b][b]f32) : [b][b]f32 -> transpose (x)) a0s
  let a2s =
    map (\a1 ->
           map (\row0 ->
                  -- Upper
                  #[unsafe]
                  loop row = copy row0
                  for i < b do
                    let sum =
                      loop sum = 0.0f32
                      for k < i do
                        sum + diag[i, k] * row[k]
                    let row[i] = row[i] - sum
                    in row)
               a1)
        a1s
  in map transpose a2s

def lud_perimeter_lower [b] [m] (diag: [b][b]f32) (mat: [m][b][b]f32) : *[m][b][b]f32 =
  map (\blk ->
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

def block_size : i64 = 32

def main [num_blocks] [n] (matb: *[num_blocks][num_blocks][n][n]f32) b =
  #[unsafe]
  let matb =
    loop matb for step < (n / b) - 1 do
      let diag = lud_diagonal matb[step, step]
      let row_slice = matb[step, step + 1:num_blocks]
      let top_per_irreg = lud_perimeter_upper diag row_slice
      let col_slice = matb[step + 1:num_blocks, step]
      let lft_per_irreg = lud_perimeter_lower diag col_slice
      let inner_slice = matb[step + 1:num_blocks, step + 1:num_blocks]
      let matb[step, step] = diag
      let matb[step, step + 1:num_blocks] = top_per_irreg
      in matb
  in matb
