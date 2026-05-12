-- Complicated horizontal fusion between reductions and scans.
-- ==
-- input { [1,2,-3,4,0,4] }
-- output { 8i32 [false, false, false, false, false, false] 0i32 }
-- structure { Screma 1 }
-- structure gpu { SegRed 1 SegScan 1 }

def main (xs: []i32) =
  ( reduce (+) 0 xs
  , scan (&&) true (map (< 0) xs)
  , reduce (*) 0 xs
  )
