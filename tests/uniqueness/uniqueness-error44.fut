-- A lambda function must not return a global array.
-- ==
-- error: aliases the free variable `global`

let global: []i32 = [1,2,3]

let main = \(b: bool) -> if b then global else []
