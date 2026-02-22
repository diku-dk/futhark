-- ==
-- input { 9i32 } output { [1, 9, 0, 4] }

def main (y: i32) =
  let xs = {f = [1, 2, 3, 4i32]}
  let xs.f[1:3] = [y, 0]
  in xs.f
