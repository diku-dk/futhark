-- ==
-- input { [[7i32, 10i32, 2i32, 4i32, 3i32, 1i32, 8i32],
--          [4i32, 4i32, 0i32, 9i32, 9i32, 0i32, 1i32],
--          [3i32, 2i32, 5i32, 10i32, 0i32, 5i32, 0i32]] }
-- output { [[8i32, 11i32, 3i32, 5i32, 4i32, 2i32, 9i32],
--           [5i32, 5i32, 1i32, 10i32, 10i32, 1i32, 2i32],
--           [4i32, 3i32, 6i32, 11i32, 1i32, 6i32, 1i32]] }

def main [n] [m] (rss: *[n][m]i32) : [][]i32 =
  map (\(rs: []i32) ->
         loop rs = copy rs
         for i < m do
           let rs[i] = rs[i] + 1
           in rs)
      rss
