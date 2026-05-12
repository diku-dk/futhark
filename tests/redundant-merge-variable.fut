-- Test that we can remove an unused loop result as well as the
-- computation that creates it.
--
-- ==
-- structure { Loop/Negate 0 }

def main (a: *[]i32, b: *[]i32, n: i32) : []i32 =
  (loop ((a, b)) for i < n do
     let a[i] = a[i] + 1
     let b[i] = -b[i]
     in (a, b)).0
