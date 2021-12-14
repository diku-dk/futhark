-- Range with known and wrong size.
-- ==
-- error: n \+ 1

def main (n: i64) : [n]i32 =
  0..<(n+1)
