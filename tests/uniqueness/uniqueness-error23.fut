-- ==
-- error: .*consumed.*

def g(ar: *[]i64, a: *[][]i64): i64 =
  ar[0]

def f(ar: *[]i64, a: *[][]i64): i64 =
  g(a[0], a) -- Should be a type error, as both are supposed to be unique

def main(n: i64): i64 =
  let a = copy(replicate n (iota n))
  let ar = copy(a[0]) in
  f(ar, a)
