-- ==
-- error: Cannot apply "==" to "b\[0\]"

def main (b: bool) (xs: []i32) =
  let a = [] : [][]i32
  let b = [filter (>0) xs]
  in a[0] == b[0]
