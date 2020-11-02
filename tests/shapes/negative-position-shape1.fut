-- It should not be allowed to have a shape parameter that is only
-- used in negative position in the parameter types, but only if that
-- size is unambiguous.
-- ==
-- error: ambiguous

let f [n] (g: [n]i64 -> i64) : i64 = n

let main = f (\xs -> xs[0])
