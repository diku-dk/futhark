-- ==
-- input  { true 20 }
-- output { [11, 12, 13, 14, 15, 16, 17, 18, 19] }
--
-- input  { false 20 }
-- output { empty([0]i32) }
let main (b: bool) (n: i32) =
    if b then filter (>10) (iota n) else []
