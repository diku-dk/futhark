-- For-in loop where replicate should be optimised away.
-- ==
-- input { 5i64 }
-- output { 99i64 }
-- structure { Replicate 0 }

def main (n: i64) =
  let xs = replicate n n
  in loop a = 0 for x in xs do (a << 1) ^ x
