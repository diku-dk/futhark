-- Respect sizes based on named parameters.
-- ==
-- error: "n"

let ap (f: (n: i64) -> [n]i32) (k: i64) : [k]i32 =
  f k

let main = ap (\n -> iota (n+1)) 10
