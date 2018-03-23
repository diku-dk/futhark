-- Array type ascription cannot change the rank of an array.
--
-- ==
-- error: Couldn't match

let main [n][m] (x: [n][m]i32) = x : []i32
