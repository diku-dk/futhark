-- More tricky variant of loop0.fut where expanding the initial merge
-- parameter values is not so simple.
--
-- ==
--
-- input {
--   [[[1,7],[9,4],[8,6]],
--    [[1,0],[6,4],[1,6]]]
-- }
-- output {
--   [[19, 24],
--    [9, 10]]
-- }
--
-- structure distributed { Map/Loop 0 }

fun [n][k]int main([n][m][k]int a) =
  map(fn [k]int ([m][k]int a_r) =>
        let acc = a_r[0] in
        loop(acc) = for i < m do
          zipWith(+, acc, a_r[i]) in
        acc
     , a)
