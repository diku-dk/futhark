-- This test fails with a DuplicateDefinition error.
-- ==
-- error: .*Dup.*

type foo = int

struct Foo
  {
    fun foo foo() = 1
  }
type foo = float

fun int main() = 0
