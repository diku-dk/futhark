-- This test tracks whether aliasing is propagated properly when
-- tuples of differing dimensions is used as function parameters.
-- ==
-- error: "b".*consumed

let f(x: (i32, i32), t: (i32, i32, []i64)): []i64 =
    let (x, y, a) = t in
    a

let main(): []i32 =
    let n = 10
    let a = iota(n)
    let t = (3, 4, a)
    let b = f((1,2), t)
    let a[0] = 2 in
    b -- Error, because b is aliased to t.
