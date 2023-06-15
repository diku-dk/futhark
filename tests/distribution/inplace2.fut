-- Ensure that in-place updates with variant indexes/values are
-- distributed sensibly.
-- ==
-- input { [[1,2], [3,4]] [0,1] [42,1337] }
-- output { [[42,2], [3,1337]] }
-- structure { Replicate 0 }

def main (xss: *[][]i32) (is: []i32) (vs: []i32) =
  map3 (\(xs: []i32) i v -> copy xs with [i] = v) xss is vs
