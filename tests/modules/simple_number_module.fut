-- ==
-- input {
--   10 21
-- }
-- output {
--   6
-- }

type t = i32
module NumLib  {
    fun plus(a: t, b: t): t = a + b
  module BestNumbers
    {
      fun four(): t = 4
      fun seven(): t = 42
      fun six(): t = 41
    }
  }


fun localplus(a: i32, b: i32): i32 = NumLib.plus (a,b)

fun main(a: i32, b: i32): i32 =
  localplus(NumLib.BestNumbers.four() ,   2)
