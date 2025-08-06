-- Nested concatenation with an invariant part.
-- ==
-- input { [[1,2],[3,4]] }
-- output { [[1, 2, 2, 2], [3, 4, 2, 2]]}
-- structure gpu { Kernel 0 }

def main [n] (xss: [n][]i32) =
  let m = n + 2
  in map (\xs -> concat xs (replicate 2 2) :> [m]i32) xss
