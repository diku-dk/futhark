-- ==
-- input {
--   42
-- }
-- output {
--   820
-- }
fun main(n: int): int =
  let a = iota(1) in
  loop (a) = for i < n do
               let b = replicate(n, 0) in -- Error if hoisted outside loop.
               loop (c=b) = for j < i do
                            let c[0] = c[0] + j in c
               in c
  in a[0]
