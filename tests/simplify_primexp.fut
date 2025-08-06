-- The map should be simplified away entirely, even though it is a
-- call to a built-in function.
-- ==
-- structure gpu { SegMap 1 }

def main (n: i64) (accs: []i64) =
  let ys = map (2 **) (iota n)
  in map (\acc -> loop acc for y in ys do acc * y) accs
