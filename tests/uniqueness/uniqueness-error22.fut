-- Test that we cannot consume anything inside an anonymous function.
-- ==
-- error: Consuming variable "a"

def f (a: *[]i64) = a[0]

def main (n: i64) =
  let a = iota (n)
  in foldl (\sum i -> sum + f (a)) 0 (iota (10))
