-- ==
-- input {
--   10 21
-- }
-- output {
--   6
-- }

type t = int
module NumLib  {
    fun plus(a: t, b: t): t = a + b
  module BestNumbers
    {
      fun four(): t = 4
      fun seven(): t = 42
      fun six(): t = 41
    }
  }


fun localplus(a: int, b: int): int = NumLib.plus (a,b)

fun main(a: int, b: int): int =
  localplus(NumLib.BestNumbers.four() ,   2)
