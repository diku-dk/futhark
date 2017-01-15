-- The module opens a new environment, which lets us use names again, which were used
-- in a previous scope.
-- ==
-- input { }
-- output { 1 2.0 }

type foo = i32
module Foo
  {
    fun foo(): i32 = 1
    module Foo
      {
        type foo = f64
        fun foo(): foo = 2.0
      }
  }

fun main(): (foo, Foo.Foo.foo) = ( Foo.foo() , Foo.Foo.foo())