-- Only one of the accesses should be unsafe.
--
-- ==
-- structure { Assert 1 }

fun main(a: []int, i: int, j: int): (int,int) =
  (unsafe a[i], a[j])
