-- Test that write works for arrays of tuples.
-- ==
--
-- input {
--   [1]
--   [1]
--   [1337]
--   [[1,2,3],[4,5,6],[7,8,9]]
--   [[10,20,30],[40,50,60],[70,80,90]]
-- }
-- output {
--   [[1,2,3],[4,1337,6],[7,8,9]]
--   [[10,20,30],[40,1337,60],[70,80,90]]
-- }

-- let scatter_blab 't [n] [h] [w] (dest: *[h][w]t) (is: [n](i64, i64)) (vs: [n]t): *[h][w]t =
--   let len = h * w
--   let is' = map (\(i, j) -> if i < 0 || i >= h || j < 0 || j >= w
--                             then -1
--                             else i * w + j)
--                 is
--   let flattened = flatten_to len dest
--   in scatter flattened is' vs |> unflatten h w

def main [k] [n] [m]
         (indexes1: [k]i32)
         (indexes2: [k]i32)
         (values: [k]i32)
         (array1: *[n][m]i32)
         (array2: *[n][m]i32) : ([n][m]i32, [n][m]i32) =
  unzip (map unzip (scatter_2d (copy (map2 zip array1 array2)) (zip (map i64.i32 indexes1) (map i64.i32 indexes2)) (zip values values)))
