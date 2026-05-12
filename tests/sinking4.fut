-- Avoid sinking when the value is actually used afterwards in a
-- result. See issue #858.
-- ==

def main (arr: [](i32, i32, i32, i32, i32)) =
  let (a, b, c, d, e) = arr[0]
  let x = if a == 0 then 0 else b + c + d + e
  in (x, a, b, c, d, e)
