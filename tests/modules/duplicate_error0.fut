-- This test fails with a DuplicateDefinition error.
-- ==
-- error: .*Dup.*

module Foo
  {
    fun foo(): i32 = 1
  }
fun bar(): i32 = 1
fun bar(): i32 = 2

fun main(): i32 = 0
