-- This test fails with a DuplicateDefinition error.
-- ==
-- error: .*Dup.*

fun int bar() = 1
struct foo 
  {
    fun foo foo() = 1
  }
fun int bar() = 2

fun int main() = 0
