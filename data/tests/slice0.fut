-- Test of basic slicing.
--
-- ==
-- input { [1,2,3,4,5] 1 3 }
-- output { [2,3] }
-- input { [1,2,3,4,5] 1 1 }
-- output { empty(int) }
-- input { [1,2,3,4,5] 1 0 }
-- error: Assertion.*failed

fun []int main([n]int as, int i, int j) =
  as[i:j]
