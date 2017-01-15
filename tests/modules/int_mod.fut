-- ==
-- input {
--   10 21
-- }
-- output {
--   31
-- }

module IntLib  {
    fun plus(a: i32, b: i32): i32 = a + b
    fun numberFour(): i32 = 4
  }

fun localplus(a: i32, b: i32): i32 = IntLib.plus (a,b)

fun main(a: i32, b: i32): i32 =
  localplus(a,b)
