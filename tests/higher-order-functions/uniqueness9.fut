-- This requires care to maintain the right uniqueness attributes.

def singleton (f: i32 -> []i32) : ([]i32, *[]i32) =
  let xs = f 1
  in (xs, [1])

def main (xs: []i32) =
  singleton (\y -> if y >= 0 then [xs[y]] else xs)
