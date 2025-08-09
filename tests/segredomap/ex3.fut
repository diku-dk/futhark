-- A redomap, where the "map" part turns a 1D list into a 2D list
-- ==
-- input {
--   [[1, 2, 3], [4, 5, 6]]
--   [-5, 10]
-- }
-- output {
--   [6, 15]
--   [ [ [-4, 11], [-3, 12], [-2, 13] ]
--   , [ [-1, 14], [ 0, 15], [ 1, 16] ]
--   ]
-- }

--
def main [m] [n] [l] (xss: [m][n]i32) (ys: [l]i32) : ([m]i32, [m][n][l]i32) =
  unzip (map (\(xs: [n]i32) : (i32, [n][l]i32) ->
                let zs = map (\x -> map (\y -> x + y) ys) xs
                in (reduce (+) 0 xs, zs))
             xss)
