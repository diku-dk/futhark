-- When distributing this program we should interchange the outer
-- parallel loop with the inner sequential one.  The loop is just a
-- sequentially written reduction with zipWith(+).
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

fun [n][k]int main([n][m][k]int a) =
  let acc = replicate(k, 0) in
  map(fn [k]int ([m][k]int a_r) =>
        loop(acc) = for i < m do
          zipWith(+, acc, a_r[i]) in
        acc
     , a)
