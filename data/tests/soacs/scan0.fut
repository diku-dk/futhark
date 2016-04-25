-- Big prefix sum on iota.
--
-- For result simplicity, we only return the last element.
--
-- ==
-- tags { no_python }
-- input { 100 }       output { 4950 }
-- compiled input { 100000000 } output { 887459712 }
-- structure distributed { ScanKernel 2 Iota 0 }

fun int main(int n) =
  let a = scan(+, 0, iota(n))
  in a[n-1]
