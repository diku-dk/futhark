-- A size goes out of scope.
-- ==
-- input { 2i64 }
-- output { [0i64,1i64,2i64] }

def main (n: i64) : [n + 1]i64 =
  let m = n + 1
  in iota m
