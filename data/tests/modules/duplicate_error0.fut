-- This test fails with a DuplicateDefinition error.
-- ==
-- error: .*Dup.*

struct Foo
  {
    fun foo(): int = 1
  }
fun bar(): int = 1
fun bar(): int = 2

fun main(): int = 0
