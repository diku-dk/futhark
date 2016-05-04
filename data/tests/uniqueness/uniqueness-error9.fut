-- This test tracks whether aliasing is propagated properly when
-- tuples of differing dimensions is used as function parameters.
-- ==
-- error:

fun [int] f((int, int) x, (int, int, [int]) t) =
    let (x, y, a) = t in
    a

fun [int] main() =
    let n = 10 in
    let a = iota(n) in
    let t = (3, 4, a) in
    let b = f((1,2), t) in
    let a[0] = 2 in
    b -- Error, because b is aliased to t.
