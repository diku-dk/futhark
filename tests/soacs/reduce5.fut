-- Reduction with an array accumulator, where the compiler (probably)
-- cannot do a interchange.
--
-- ==
-- input { [[1,2,3], [4,5,6], [6,7,8]] }
-- output { [11i32, 14i32, 17i32] }

def main [n] [m] (xss: [n][m]i32) : []i32 =
  reduce_comm (\(xs: []i32) ys ->
                 loop zs = replicate m 0
                 for i < m do
                   let zs[i] = xs[i] + ys[i]
                   in zs)
              (replicate m 0)
              xss
