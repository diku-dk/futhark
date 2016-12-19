-- A better name would be 'take'...
-- ==
-- input { 2 [1,2,3,4,5] }
-- output { [1,2] }
fun main(n: int, a: []int): []int =
  #0 (split (n) a)
