-- Size parameters are only allowed for things that have parameters.
-- ==
-- error: Size parameter

let iiota [n] : [n]i32 = iota n
