-- ==
-- input { 10i64 }
-- error: issue1758.fut:8

def main (i: i64) : [i]i64 =
  let a = iota i
  let b = replicate i 0
  in a with [:4] = b
