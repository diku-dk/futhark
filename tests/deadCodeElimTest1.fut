-- ==
-- input {
--   10
-- }
-- output {
--   -1
-- }
let neg(x: i32): i32 = -x

let main(a: i32): i32 =
  let b = a + 100
  let x = iota(a)
  let c = b + 200
  let z = 3*2 - 6
  --let y = map(op ~, x) in
  let y = map neg x
  let d = c + 300      in
    if(false)
    then d + y[1]
    else y[1]
