-- ==
-- error: cannot match

def main  =
  let [n] (A: [n]i64, B: [n]i64) = (iota 1, iota 2)
  in zip A B
