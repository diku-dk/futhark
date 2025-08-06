-- For-in loop where map and iota should be optimised away.
-- ==
-- input { 5i64 }
-- output { 2i64 }
-- structure { Iota 0 Map 0 }

def main (n: i64) =
  let xs = map (2 *) (map (1 +) (iota n))
  in loop a = 0 for x in xs do a ^ x
