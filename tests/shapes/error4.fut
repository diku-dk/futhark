-- We cannot just ignore constraints imposed by a higher-order function.
-- ==
-- error: Dimensions.*`n`.*do not match

let f (g: (n: i32) -> [n]i32) (l: i32): i32 =
  (g l)[0]

let main = f (\n : []i32 -> iota (n+1))
