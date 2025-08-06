-- Single-iteration maps should be simplified away.
--
-- ==
-- input { 2 } output { [4] }
-- structure { Map 0 }

def main (x: i32) : [1]i32 =
  map (+ x) (replicate 1 x)
