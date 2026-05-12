-- Sumtype aliasing.
-- ==
-- error: "xs".*consumed

type sum [n] = #foo ([n]i32) | #bar ([n]i32)

def main [n] (xs: *[n]i32) =
  let v: sum [n] = #foo xs
  let xs[0] = 0
  let v' = v
  in 0
