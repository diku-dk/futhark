-- ==
-- input {
--   10 21
-- }
-- output {
--   31
-- }

module IntLib = {
  let plus(a: i32, b: i32): i32 = a + b
  let numberFour(): i32 = 4
}

let localplus(a: i32, b: i32): i32 = IntLib.plus (a,b)

let main(a: i32, b: i32): i32 =
  localplus(a,b)
