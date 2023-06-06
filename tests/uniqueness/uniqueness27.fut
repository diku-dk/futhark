def main b1 b2 (A: *[]i32) (B: *[]i32) (C: *[]i32) =
  let X = if b1 then A else B
  let Y = if b2 then B else C
  let X[0] = 0
  -- This is OK because while X aliases {A,B} and Y aliases {B,C},
  -- there is no way for the consumption of X to touch C.
  in (X, C)
