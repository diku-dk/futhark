-- This test is written to ensure that the same name can be used
-- for different declarations in the same local environment, as long as their types does not overlap.
-- ==
-- input { 4 }
-- output { 4 }

type foo = int
fun foo foo(int a) = a + a
struct Foo
  {
    fun int one() = 1
  }

fun int main(int x) = x