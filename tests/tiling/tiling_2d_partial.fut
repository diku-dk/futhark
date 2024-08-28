-- 2D tiling when not all of the arrays are invariant.
-- ==
-- compiled random input {
--   [256][256]i32
--   [256][256]i32
--   [256][256][256]i32
--   [256]i32
-- } auto output
-- compiled random input {
--   [256][10]i32
--   [10][256]i32
--   [256][10][256]i32
--   [10]i32
-- } auto output
-- structure gpu { SegMap/Loop/SegMap 2 }

def main [a][b][c] (xs: [a][c]i32) (ys: [c][b]i32) (zsss: [b][c][a]i32) (vs: [c]i32) =
  map2 (\xs' zss ->
          map2 (\ys' zs ->
                  #[sequential]
                  i32.sum (map2 (+)
                                vs
                                (map2 (*) zs (map2 (*) xs' ys'))))
               (transpose ys) zss)
       xs (transpose (map transpose zsss))
