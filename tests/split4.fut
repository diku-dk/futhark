-- A better name would be 'take'...
-- ==
-- input { 2 [1,2,3,4,5] }
-- output { [1,2] }
fun main(n: i32, a: []i32): []i32 =
  #0 (split (n) a)
