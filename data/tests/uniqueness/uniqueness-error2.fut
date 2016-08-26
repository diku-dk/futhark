-- Test that non-basic aliasing of an array results in an aliased
-- array.
-- ==
-- error:

fun main(): []int =
    let n = 10 in
    let a = copy(replicate(n, iota(n))) in -- Note that a is 2-dimensional
    let b = a[0] in -- Now b aliases a.
    let a[1] = replicate(n, 8) in -- Consume a, thus also consuming b.
    b -- Error!
