-- ==
-- input { 2i64 }
-- output { [0i64] [-1] }

def main (n: i64) =
  let foo = iota (n - 1)
  let bar = replicate (n - 1) (-1)
  in (foo, bar)
