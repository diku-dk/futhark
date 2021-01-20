-- Errors in constants must be detectable.
-- ==
-- input { 2 }
-- error: false

let bad = assert false 0i32

let main x = x + bad
