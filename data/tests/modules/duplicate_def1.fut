-- The struct opens a new environment, which lets us use names again, which were used
-- in a previous scope.
-- ==
-- input { }
-- output { 1 2.0 }

type foo = int
struct Foo
  {
    fun foo(): int = 1
    struct Foo
      {
        type foo = float
        fun foo(): foo = 2.0
      }
  }

fun main(): (foo, Foo.Foo.foo) = ( Foo.foo() , Foo.Foo.foo())