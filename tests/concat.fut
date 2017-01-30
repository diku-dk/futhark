-- ==
-- input {
-- }
-- output {
--   6
-- }
fun main(): i32 =
  let a = [(1,(2,3))]
  let c = concat a a
  let (x,(y,z)) = c[1] in
  x+y+z
