-- A consumed argument may not alias another argument, even through a name that
-- has gone out of scope.
-- ==
-- error: Argument is consumed, but aliases

def f (a: *[]i32) (b: []i32) : i32 = let a[0] = 10 in a[0] + b[0]

def main (n: i64) : i32 =
  let (a, b) = (let t = map i32.i64 (iota n) in (t, t))
  in f a b
