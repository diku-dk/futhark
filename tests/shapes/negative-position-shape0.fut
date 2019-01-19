-- It should not be allowed to have a shape parameter that is only
-- used in negative position in the parameter types.
-- ==
-- error: Shape parameter `n` must first be given .*

let f [n] (g: i32 -> [n]i32) : i32 = n
