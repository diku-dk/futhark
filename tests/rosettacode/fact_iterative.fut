-- ==
-- input {
--   10
-- }
-- output {
--   3628800
-- }
let fact(n: i32): i32 =
  loop out = 1 for i < n do
    out * (i+1)

let main(n: i32): i32 =
  fact(n)
