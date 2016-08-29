-- ==
-- input {
--   10
-- }
-- output {
--   -1
-- }
fun neg(x: int): int = -x

fun main(a: int): int =
  let b = a + 100
  let x = iota(a)
  let c = b + 200
  let z = 3*2 - 6
  --let y = map(op ~, x) in
  let y = map neg x
  let d = c + 300      in
    if(False)
    then d + y[1]
    else y[1]
