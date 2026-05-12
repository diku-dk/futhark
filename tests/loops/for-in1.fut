-- For-in loop where iota should be optimised away.
-- ==
-- input { 5i64 }
-- output { 4i64 }
-- structure { Iota 0 }

def main (n: i64) =
  let xs = iota n
  in loop a = 0 for x in xs do a ^ x
