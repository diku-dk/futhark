-- ==
-- input { [3i64, 5i64, 9, 1] }
-- auto output
def main [n] (xs: [n]i64) =
  map (\x ->
         let mat = opaque (iota x)
         let mat2 = map (+5) mat
         in mat2[0])
      xs

-- -- ==
-- -- input { [3i64, 5i64] }
-- -- auto output
-- def main [n] (xs: [n]i64) =
--   map (\x ->
--          let mat = map (\i -> map (\j -> i + j) (iota x)) (iota x)
--          let mat2 =
--            map (\row ->
--                   let z = row[0]
--                   in map (\e -> e + z) row)
--                mat
--          in mat2[0][0])
--       xs
