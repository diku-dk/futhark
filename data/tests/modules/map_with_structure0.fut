-- Testing whether it is possible to use a function
-- from a struct in a curry function (map)
-- ==
-- input {
--   [1, 2, 3 ,4, 5, 6, 7, 8, 9, 10] 
-- }
--  output {
--  55
-- }

struct F  {
    fun int plus(int a, int b) = a+b
  }

fun int main([]int a) = reduce(F.plus , 0 , a)
