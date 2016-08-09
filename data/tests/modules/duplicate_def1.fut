-- The struct opens a new environment, which lets us use names again, which were used
-- in a previous scope.
-- ==
-- input { }
-- output { 1 2.0 }

type foo = int
struct Foo
  {
    fun int foo() = 1
    struct Foo
      {
        type foo = float
        fun foo foo() = 2.0
      }
  }

fun (foo, Foo.Foo.foo) main() = ( Foo.foo() , Foo.Foo.foo())