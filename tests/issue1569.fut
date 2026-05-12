def dotprod [n] (a: [n]f32) (b: [n]f32) : f32 =
  map2 (*) a b |> reduce (+) 0

def lud_diagonal [b] (a: [b][b]f32) : *[b][b]f32 =
  let mat = copy a
  in loop (mat: *[b][b]f32) for i < b - 1 do
       let col =
         map (\j ->
                if j > i
                then #[unsafe] (mat[j, i] - (dotprod mat[j, :i] mat[:i, i])) / mat[i, i]
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

def lud_perimeter_upper [m] (diag: [16][16]f32, a0s: [m][16][16]f32) : *[m][16][16]f32 =
  let a1s = map (\(x: [16][16]f32) : [16][16]f32 -> transpose (x)) a0s
  let a2s =
    map (\a1 : [16][16]f32 ->
           map (\row0 : [16]f32 ->
                  -- Upper
                  loop row = copy row0
                  for i < 16 do
                    let sum = (loop sum = 0.0f32 for k < i do sum + diag[i, k] * row[k])
                    let row[i] = row[i] - sum
                    in row)
               a1)
        a1s
  in map (\x : [16][16]f32 -> transpose (x)) a2s

def lud_perimeter_lower [m] (diag: [16][16]f32, mat: [m][16][16]f32) : *[m][16][16]f32 =
  map (\blk : [16][16]f32 ->
         map (\(row0: [16]f32) : *[16]f32 ->
                -- Lower
                loop row = copy row0
                for j < 16 do
                  let sum =
                    loop sum = 0.0f32
                    for k < j do
                      sum + diag[k, j] * row[k]
                  let row[j] = (row[j] - sum) / diag[j, j]
                  in row)
             blk)
      mat

def main [num_blocks] (matb: *[num_blocks][num_blocks][16][16]f32) : [num_blocks][num_blocks][16][16]f32 =
  let step = 0
  let diag = lud_diagonal (matb[step, step])
  let matb[step, step] = diag
  let row_slice = matb[step, step + 1:num_blocks]
  let top_per_irreg = lud_perimeter_upper (diag, row_slice)
  let matb[step, step + 1:num_blocks] = top_per_irreg
  let col_slice = matb[step + 1:num_blocks, step]
  let lft_per_irreg = lud_perimeter_lower (diag, col_slice)
  let matb[step + 1:num_blocks, step] = lft_per_irreg
  in matb
