-- Check that lookup of multiple modules in 'open' is simultaneous,
-- not parallel.
-- ==
-- input { }
-- output { 1 3 }

module M1 = {
  val x = 1
  module M2 = {
    val y = 2
  }
}

module M2 = {
  val y = 3
}

open M1 M2

fun main() = (x, y)