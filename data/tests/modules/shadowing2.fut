-- M0.foo() changes meaning inside M1, after the previous declaration of M0
-- is overshadowed.
-- 
-- ==
-- input {
-- }
-- output {
--  12 
-- }

struct M0 
  {
    fun int foo() = 1
  }

struct M1 
  {
    fun int bar() = M0.foo()
    struct M0 
      {
        fun int foo() = 10
      }
    fun int baz() = M0.foo()
  }
  
fun int main() = M0.foo() + M1.bar() + M1.baz()