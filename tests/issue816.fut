-- ==
-- input { [[[1f32, 2f32, 3f32], [4f32, 5f32, 6f32], [7f32, 8f32, 9f32]]] }
-- output { [[[1.0f32, 2.0f32, 3.0f32], [1.0f32, 2.0f32, 3.0f32], [2.0f32, 4.0f32, 6.0f32]]] }
def main [m] [b] (peri_batch_mat: [m][b][b]f32) =
  map (\peri_mat ->
         let mat = copy peri_mat
         in loop mat for im1 < (b - 1) do
              #[unsafe]
              let i = im1 + 1
              let row_sum =
                loop row_sum = replicate b 0
                for j < i do
                  map2 (+) row_sum mat[j]
              let mat[i] = row_sum
              in mat)
      peri_batch_mat
