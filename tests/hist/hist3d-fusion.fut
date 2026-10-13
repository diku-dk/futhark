-- Two 3d histograms over the same indexes must fuse horizontally.
-- ==
-- input {
--   [
--     [[0,0],[0,0]],
--     [[0,0],[0,0]]
--   ]
--   [
--     [[0,0],[0,0]],
--     [[0,0],[0,0]]
--   ]
--   [0i64, 7i64, 0i64]
--   [1,2,3]
-- }
-- output {
--   [
--     [[4,0],[0,0]],
--     [[0,0],[0,2]]
--   ]
--   [
--     [[8,0],[0,0]],
--     [[0,0],[0,4]]
--   ]
-- }
-- structure { Hist 1 }

def main [n] [m] [k] [l]
         (a: *[n][m][k]i32)
         (b: *[n][m][k]i32)
         (r: [l]i64)
         (v: [l]i32) : (*[n][m][k]i32, *[n][m][k]i32) =
  let is = map (\x -> (x / (m * k), (x / k) % m, x % k)) r
  in ( reduce_by_index_3d a (+) 0 is v
     , reduce_by_index_3d b (+) 0 is (map (* 2) v)
     )
