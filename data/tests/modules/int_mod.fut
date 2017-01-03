-- ==
-- input {
--   10 21
-- }
-- output {
--   31
-- }

module IntLib  {
    fun plus(a: int, b: int): int = a + b
    fun numberFour(): int = 4
  }

fun localplus(a: int, b: int): int = IntLib.plus (a,b)

fun main(a: int, b: int): int =
  localplus(a,b)
