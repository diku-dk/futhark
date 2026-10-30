-- ==
-- input  { true 20i64 }
-- output { [11i64, 12i64, 13i64, 14i64, 15i64, 16i64, 17i64, 18i64, 19i64] }
--
-- input  { false 20i64 }
-- output { empty([0]i64) }
def main (b: bool) (n: i64) =
  if b then filter (> 10) (iota n) else []
