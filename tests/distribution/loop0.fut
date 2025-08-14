-- When distributing this program we should interchange the outer
-- parallel loop with the inner sequential one.  The loop is just a
-- sequentially written reduction with map((+)).
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
-- structure gpu { Map/Loop 0 }

def main [n] [m] [k] (a: [n][m][k]i32) : [n][k]i32 =
  let acc = replicate k 0
  in map (\(a_r: [m][k]i32) : [k]i32 ->
            loop (acc) for i < m do
              map2 (+) acc (a_r[i]))
         a
