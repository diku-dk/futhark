-- We should be able to handle a for-loop with negative bound.
-- ==
-- input { 1 } output { 1 }
-- input { -1 } output { 0 }

let main (n: i32) = loop x = 0i32 for _i < n do x + 1
