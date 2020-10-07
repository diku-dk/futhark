-- ==
-- input {
-- }
-- output {
--   0i64
-- }
let f(a: *[]i64) = a[0]
let g(a: []i64) = a[0]

let main: i64 =
    let n = 10
    let a = iota(n)
    let b = a in
    if 1 == 2 then let c = g(b) in f(a) + c
              else let c = g(a) in f(b) -- OK Because only one branch is taken.
