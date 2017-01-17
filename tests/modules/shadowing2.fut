-- M0.foo() changes meaning inside M1, after the previous declaration of M0
-- is overshadowed.
--
-- ==
-- input {
-- }
-- output {
--  1 1 10
-- }

module M0 = {
  fun foo(): i32 = 1
}

module M1 = {
  fun bar(): i32 = M0.foo()
  module M0 = {
    fun foo(): i32 = 10
  }
  fun baz(): i32 = M0.foo()
}

fun main(): (i32, i32, i32) = (M0.foo(), M1.bar(), M1.baz())
