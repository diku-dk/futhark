-- Other ranges do not have known sizes.
-- ==
-- error: unknown length of range

def main (n: i64) : [n]i64 =
  1..2..<n + 1
