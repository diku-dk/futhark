-- ==
-- input { 9 } output { 9 }

def main (x: i32) : i32 =
  let r = {f=[1i32, 2i32, 3i32]}
  let r' = r with f[1] = x
  in r'.f[1]