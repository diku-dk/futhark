-- M0.foo() changes meaning inside M1, after the previous declaration of M0
-- is overshadowed.
--
-- ==
-- input {
-- }
-- output {
--  12
-- }

module M0
  {
    fun foo(): int = 1
  }

module M1
  {
    fun bar(): int = M0.foo()
    module M0
      {
        fun foo(): int = 10
      }
    fun baz(): int = M0.foo()
  }

fun main(): int = M0.foo() + M1.bar() + M1.baz()
