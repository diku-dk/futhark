-- ==
-- error: "a".*consumed

def f(a: *[]i64, i: i32, v: i64): i64 = let a[i]=v in a[i]

def main(): i64 =
    let n = 10
    let a = iota(n)
    let b = a -- a and b are aliases.
    let (i,j) = (2,5) in
    f(a,i,42) -- Consumes a (and b through the alias)
    + b[j] -- Error!
