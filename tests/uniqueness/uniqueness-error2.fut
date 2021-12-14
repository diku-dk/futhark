-- Test that non-basic aliasing of an array results in an aliased
-- array.
-- ==
-- error: .*consumed.*

def main(): []i32 =
    let n = 10
    let a = replicate n (iota n) -- Note that a is 2-dimensional
    let b = a[0] -- Now b aliases a.
    let a[1] = replicate n 8 in -- Consume a, thus also consuming b.
    b -- Error!
