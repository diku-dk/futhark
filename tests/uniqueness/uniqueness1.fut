-- Test that simple function argument consumption works.
-- ==
-- input {
-- }
-- output {
--   0
-- }

def f(a: *[]i64): i64 = a[0]

def main: i32 =
    let n = 10
    let b = iota(n)
    let a = b -- Alias a to b.
    let x = f(b) in -- Consumes both b and a because a is aliased to b.
    0 -- OK!
