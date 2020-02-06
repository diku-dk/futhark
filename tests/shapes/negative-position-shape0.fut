-- It should not be allowed to have a shape parameter that is only
-- used in negative position in the parameter types.
-- ==
-- error: Size parameter "n" must be used

let f [n] (g: i32 -> [n]i32) : i32 = n
