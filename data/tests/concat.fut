-- ==
-- input {
-- }
-- output {
--   6
-- }
fun main(): int =
  let a = [(1,(2,3))] in
  let c = concat a a in
  let (x,(y,z)) = c[1] in
  x+y+z
