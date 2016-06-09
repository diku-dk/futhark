-- The struct opens a new environment, which lets us use names again, which were used
-- in a previous scope.
-- ==
-- input { }
-- output { (1 , 2.0) }

type foo = int
struct foo 
  {
    fun int foo() = 1
    struct foo 
      {
        type foo = float
        fun foo foo() = 2.0
      }
  }

fun (foo, foo.foo.foo) main() = ( foo.foo() , foo.foo.foo())