-- ==
-- input {
-- }
-- output {
--   [0, 1, 3, 6, 10, 15, 21, 28, 36, 45]
-- }
let main(): []i32 =
    let n = 10
    let x = iota(n)
    in loop (x) for i < n-1 do
         let x[i+1] = x[i+1] + x[i]
         in  x
