-- M1.foo() calls the most recent declaration of number, due to M0.number()
-- being brought into scope of M1, overshadowing the top level definition of
-- number()
-- ==
-- input {
-- }
-- output {
--  2
-- }

fun int number() = 1
struct M0 
  {
    fun int number() = 2
    struct M1 
      {
        fun int foo() = number()
      }
  }

fun int main() = M0.M1.foo()