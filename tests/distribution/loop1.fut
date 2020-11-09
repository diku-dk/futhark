-- Like loop0.fut, but the sequential loop also carries a scalar
-- variable that is not mapped.
--
-- ==
--
-- input {
--   [[[1,7],[9,4],[8,6]],
--    [[1,0],[6,4],[1,6]]]
-- }
-- output {
--   [[18, 17], [8, 10]]
--   [8, 8]
-- }
--
-- structure distributed { Map/DoLoop 0 }

let main [n][m][k] (a: [n][m][k]i32): ([n][k]i32,[n]i32) =
  let acc = replicate k 0
  let accnum = 1 in
  unzip(map (\a_r ->
        loop((acc,accnum)) for i < m do
          (map2 (+) acc (a_r[i]),
           accnum + accnum)
     ) a)

-- Example of what we want - this is dead code.
let main_distributed [n][m][k] (a: [n][m][k]i32): ([n][k]i32,[n]i32) =
  let acc_expanded = replicate n (replicate k 0)
  let accnum_expanded = replicate n 1 in
  loop((acc_expanded,accnum_expanded)) for i < m do
    unzip(map3 (\acc accnum a_r  ->
                    (map2 (+) acc (a_r[i]),
                     accnum * accnum)
                 ) (acc_expanded) (accnum_expanded) a)
