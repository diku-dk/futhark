-- Extract the components of a consumable tuple, consume one, use the other.
-- ==
-- input { [0,0] [2,2] }
-- output { [1,0] [2,2] }

def f (p: *([]i32, []i32)) : ([]i32, []i32) =
  let (a, b) = p
  let a[0] = 1
  in (a, b)

def main (xs: *[]i32) (ys: *[]i32) = f (xs, ys)
