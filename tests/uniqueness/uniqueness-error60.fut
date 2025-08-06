-- ==
-- error: result of applying "f".*consumed

def f (n: i64) : ([]i64, []i64) =
  let a = iota n
  in (a, a)

def main n =
  let (a, b) = f n
  let a[0] = 0
  in (a, b)
