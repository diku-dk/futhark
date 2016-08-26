-- This test fails with a DuplicateDefinition error.
-- ==
-- error: .*Dup.*

fun bar(): int = 1
struct Foo
  {
    fun foo(): foo = 1
  }
fun bar(): int = 2

fun main(): int = 0
