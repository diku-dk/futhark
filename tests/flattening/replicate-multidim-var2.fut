-- ==
-- input { 5i64 4i64 }
-- auto output
def main (m: i64) (n: i64) =
  map (\j ->
         let ds = iota j
         let fs = map (\d -> d + 3) ds
         let tmp = replicate m (replicate n fs) 
         let mat_3d = tmp with [0][0][0] = ds[0] + 3
        --  let mat_3d_2 =
        --    map (\mat ->
        --           let val_3d = mat[0][0][0]
        --           in map (\row -> map (\elem -> elem + val_3d) row) mat) mat_3d
         in mat_3d[0][0][0])
      (map (+3) (iota n))
