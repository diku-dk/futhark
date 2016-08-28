-- Big prefix sum on iota.
--
-- For result simplicity, we only return the last element.
--
-- ==
-- tags { no_python }
-- input { 100 }       output { 4950 }
-- compiled input { 1000000 } output { 1783293664i32 }
-- structure distributed { ScanKernel 2 Iota 0 }

fun main(n: int): int =
  let a = scan((+), 0, iota(n))
  in a[n-1]
