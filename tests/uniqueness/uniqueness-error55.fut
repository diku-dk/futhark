-- ==
-- error: "y".*consumed

def main n =
  let (a, b) =
    let y = iota n
    in (y, y)
  let a[0] = 0
  let b[0] = 0
  in (a, b)
