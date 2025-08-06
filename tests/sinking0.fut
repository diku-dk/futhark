-- ==
-- structure gpu { /Index 1 }

def main (arr: [](i32, i32, i32, i32, i32)) =
  let (a, b, c, d, e) = arr[0]
  in if a == 0 then 0 else b + c + d + e
