-- Testing whether it is possible to use a function
-- from a struct in a curry function (map)
-- ==
-- input {
--   [1, 2, 3 ,4, 5, 6, 7, 8, 9, 10] 
-- }
--  output {
--  55
-- }

struct F {
    fun plus(a: int) (b: int): int = a+b
  }

fun main(a: []int): int = reduce(F.plus , 0 , a)
