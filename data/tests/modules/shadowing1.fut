-- M1.foo() calls the most recent declaration of number, due to M0.number()
-- being brought into scope of M1, overshadowing the top level definition of
-- number()
-- ==
-- input {
-- }
-- output {
--  6.0 6 6
-- }

type best_type = float
fun best_number(): best_type = 6.0
struct M0
  {
    type best_type = int
    fun best_number(): best_type = 6
    struct M1
      {
        fun best_number(): best_type = 6
      }
  }

fun main(): (float, int, int) = (best_number() , M0.best_number() , M0.M1.best_number())