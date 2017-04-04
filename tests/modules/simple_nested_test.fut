-- ==
-- input {
--   10 21
-- }
-- output {
--   6
-- }

type t = i32
module NumLib = {
  let plus(a: t, b: t): t = a + b
  module BestNumbers = {
    let four(): t = 4
    let seven(): t = 42
    let six(): t = 41
  }
}


let localplus(a: i32, b: i32): i32 = NumLib.plus (a,b)

let main(a: i32, b: i32): i32 =
  localplus(NumLib.BestNumbers.four() ,   2)
