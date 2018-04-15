-- Preserve shape annotations when eta-expanding entry points.
-- ==
-- input { 1 } output { [0] }
-- input { 2 } error:

let main: i32 -> [1]i32 = iota
