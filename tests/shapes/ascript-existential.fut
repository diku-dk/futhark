-- ==
-- input { 0i64 } output { 1i64 }
-- input { 1i64 } output { 2i64 }

def main (n: i64) =
  length (iota (n + 1) : []i64)
