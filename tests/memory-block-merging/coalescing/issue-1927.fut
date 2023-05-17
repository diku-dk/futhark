-- ==
-- input { 3i64 }
-- output { [0i64, 0i64, 1i64, 2i64, 4i64] }
-- structure gpu-mem { Alloc 2 }

def main k =
  let src = iota k
  let dst = iota 5
  in dst with [1:4] = src
