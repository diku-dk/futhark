-- When distributing this program we should interchange the outer
-- parallel loop with the inner sequential one.  The loop is just a
-- sequentially written reduction with zipWith((+)).
--
-- Expected structure:
--
-- loop
--   map
--     map
--
-- ==
--
-- input {
--   [[[1,7],[9,4],[8,6]],
--    [[1,0],[6,4],[1,6]]]
-- }
-- output {
--   [[18, 17], [8, 10]]
-- }
--
-- structure distributed { Map/DoLoop 0 }

fun main(a: [n][m][k]int): [n][k]int =
  let acc = replicate k 0 in
  map (fn (a_r: [m][k]int): [k]int  =>
        loop(acc) = for i < m do
          zipWith (+) acc (a_r[i]) in
        acc
     ) a
