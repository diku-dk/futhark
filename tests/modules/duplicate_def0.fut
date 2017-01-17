-- This test is written to ensure that the same name can be used
-- for different declarations in the same local environment, as long as their types does not overlap.
-- ==
-- input { 4 }
-- output { 4 }

type foo = i32
fun foo(a: i32): foo = a + a
module Foo = {
  fun one(): i32 = 1
}

fun main(x: i32): i32 = x