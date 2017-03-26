-- Constants cannot be used for return type shape declarations in
-- entry points.
--
-- ==
-- error:

val n: i32 = 3

let main(): [n]i32 = replicate n 0
