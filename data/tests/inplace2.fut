-- In-place update with a slice.
--
-- ==
-- input { [1,2,3,4,5] [8,9] 2 }
-- output { [1,2,8,9,5] }
-- input { [1,2,3,4,5] [5,6,7,8,9] 0 }
-- output { [5,6,7,8,9] }
-- input { [1,2,3,4,5] empty(int) 0 }
-- output { [1,2,3,4,5] }
-- input { [1,2,3,4,5] empty(int) 1 }
-- output { [1,2,3,4,5] }
-- input { [1,2,3,4,5] empty(int) 5 }
-- error: Assertion.*failed
-- input { [1,2,3,4,5] [1,2,3] -1 }
-- error: Assertion.*failed

fun []int main(*[n]int as, [m]int bs, int i) =
  let as[i:i+m] = bs
  in as
