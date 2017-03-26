-- ==
-- input {
-- }
-- output {
--   0
-- }
let f(a: *[]i32): i32 = a[0]
let g(a: []i32): i32 = a[0]

let main(): i32 =
    let n = 10
    let a = iota(n)
    let b = a in
    if 1 == 2 then let c = g(b) in f(a) + c
              else let c = g(a) in f(b) -- OK Because only one branch is taken.
