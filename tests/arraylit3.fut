-- Array literals with identical elements should turn into replicates.
-- ==
-- input { 2 } output { [2,2] }
-- structure { ArrayLit 0 Replicate 1}

def main (x: i32) : []i32 =
  [x, x]
