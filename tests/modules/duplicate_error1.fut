-- This test fails with a DuplicateDefinition error.
-- ==
-- error: .*Dup.*

module Foo = {
  fun foo(): foo = 1
}
type foo = i32
type foo = float

fun main(): i32 = 0
