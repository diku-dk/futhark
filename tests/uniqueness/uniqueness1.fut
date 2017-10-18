-- Test that simple function argument consumption works.
-- ==
-- input {
-- }
-- output {
--   0
-- }

let f(a: *[]i32): i32 = a[0]

let main(): i32 =
    let n = 10
    let b = iota(n)
    let a = b -- Alias a to b.
    let x = f(b) in -- Consumes both b and a because a is aliased to b.
    0 -- OK!
