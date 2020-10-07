-- We cannot just ignore constraints imposed by a higher-order function.
-- ==
-- error: Dimensions.*"n".*do not match

let f (g: (n: i64) -> [n]i32) (l: i64): i32 =
  (g l)[0]

let main = f (\n : []i64 -> iota (n+1))
