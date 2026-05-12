-- Big prefix sum on iota.
--
-- For result simplicity, we only return the last element.
--
-- ==
-- tags { no_python }
-- input { 100i64 }       output { 4950 }
-- compiled input { 1000000i64 } output { 1783293664i32 }
-- structure gpu { SegScan 1 Iota 0 }

def main (n: i64) : i32 =
  let a = scan (+) 0 (map i32.i64 (iota (n)))
  in a[n - 1]
