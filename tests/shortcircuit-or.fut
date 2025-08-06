-- |  must be short-circuiting.
--
-- ==
-- input { 0i64 [false, false] } output { false }
-- input { 1i64 [false, false] } output { false }
-- input { 2i64 [false, false] } output { true }

def main [n] (i: i64) (bs: [n]bool) : bool =
  i >= n || bs[i]
