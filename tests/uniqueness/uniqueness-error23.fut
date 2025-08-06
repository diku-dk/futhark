-- ==
-- error: self-aliased

def g (ar: *[]i64, a: *[][]i64) : i64 =
  ar[0]

def f (ar: *[]i64, a: *[][]i64) : i64 =
  g (a[0], a)

-- Should be a type error, as both are supposed to be
-- unique yet they alias each other.

def main (n: i64) : i64 =
  let a = replicate n (iota n)
  let ar = copy (a[0])
  in f (ar, a)
