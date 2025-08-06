-- Using a top level size.
-- When this program failed, the problem was actually in the array literal.

def n : i64 = 20

def main (xs: []i32) =
  let ys = take n xs
  in [ys]
