-- Ensure that we cannot cheat uniqueness typing with higher-order
-- functions.
-- ==
-- error: consuming

let apply 'a 'b (f: a -> b) (x: a) = (f x, f x)

let consume (xs: *[]i32) = 0

let main (arr: *[]i32) =
 apply consume arr
