-- ==
-- input {
--   10 21
-- }
-- output {
--   31
-- }

struct IntLib  {
    fun int plus(int a, int b) = a + b
    fun int numberFour() = 4
  }

fun int localplus(int a, int b) = IntLib.plus (a,b)

fun int main(int a, int b) =
  localplus(a,b)
