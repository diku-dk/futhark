-- This test is written to ensure that the same name can be used
-- for different declarations in the same local environment, as long as their types does not overlap.
-- ==
-- input { 4 }
-- output { 4 }

type foo = int
fun foo(a: int): foo = a + a
module Foo
  {
    fun one(): int = 1
  }

fun main(x: int): int = x