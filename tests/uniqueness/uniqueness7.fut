-- ==
-- input {
-- }
-- output {
--   0
-- }
let f(a: *[][]i32): i32 = a[0,0]

let main(): i32 =
    let n = 10
    let a = replicate n (iota n)
    let b = replicate n (iota n) in
    let a = loop (a) for i < n do
                 let a[i] = b[i] in a -- Does not alias a to b, because let-with is in-place!

    let x = f(b) in -- Consumes only b.
    a[0,0] -- OK
