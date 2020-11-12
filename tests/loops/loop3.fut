-- ==
-- input {
--   42i64
-- }
-- output {
--   820i64
-- }
let main(n: i64): i64 =
  let a = iota(1) in
  let a = loop a for i < n do
             let b = replicate n 0 in -- Error if hoisted outside loop.
             loop c=b for j < i do let c[0] = c[0] + j in c
  in a[0]
