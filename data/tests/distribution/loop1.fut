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

fun ([n][k]int,[n]int) main([n][m][k]int a) =
  let acc = replicate(k, 0) in
  let accnum = 1 in
  unzip(map(fn ([k]int,int) ([m][k]int a_r) =>
        loop((acc,accnum)) = for i < m do
          (zipWith(+, acc, a_r[i]),
           accnum + accnum) in
        (acc, accnum)
     , a))

-- Example of what we want - this is dead code.
fun ([n][k]int,[n]int) main_distributed([n][m][k]int a) =
  let acc_expanded = replicate(n, replicate(k, 0)) in
  let accnum_expanded = replicate(n, 1) in
  loop((acc_expanded,accnum_expanded)) = for i < m do
    unzip(zipWith(fn ([k]int,int) ([k]int acc, int accnum, [m][k]int a_r) =>
                    (zipWith(+, acc, a_r[i]),
                     accnum * accnum)
                 , acc_expanded, accnum_expanded, a))
  in (acc_expanded,accnum_expanded)
