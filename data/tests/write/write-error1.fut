-- Fail if the index and value arrays do not have the same size.
--
-- ==
-- input { [0] [1] }
-- output { [1,0,0,0,0,0,0,0,0,0] }
-- input { [0] [1,2] }
-- error:

fun []int main([n]int is, [m]int vs) =
  let a = replicate(10, 0)
  in write(is, vs, a)
