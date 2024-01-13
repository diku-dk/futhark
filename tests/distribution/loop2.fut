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
-- structure gpu { Map/Loop 0 }

def main [n][m][k] (a: [n][m][k]i32): [n][k]i32 =
  map (\(a_r: [m][k]i32): [k]i32  ->
         let acc = a_r[0] in
         #[sequential]
        loop(acc) for i < m do
          map2 (+) acc (a_r[i])
     ) a
