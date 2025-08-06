-- Just because a top-level binding tries to hide its size (which is
-- existential), that does not mean it gets to have a blank size.
-- ==
-- input { 2i64 } output { [0i64,1i64] }

def arr : []i64 = iota (10 + 2)

def main (n: i64) =
  copy (take n arr)
