-- Don't let occurences clash just because they're function arguments.
-- ==
-- error: Cannot apply "f"

def f (a: *[]i32) : []i32 = a

def main () : ([]i32, []i32) =
  let n = 10
  let a = iota (n)
  let b = iota (n)
  let (i, j) = (2, 5)
  in (f (let a[i] = b[j] in a), f (let b[j] = a[i] in b))
