-- Don't let curried mapees consume more than once.
-- ==
-- error: consumption

def f (a: *[]i64) (i: i64) : []i64 =
  let a[i] = 0 in a

def main n =
  let a = iota (n)
  let b = iota (n)
  in map (f (a)) b

-- Bad, because a may be consumed many times.
