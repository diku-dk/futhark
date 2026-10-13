-- The components of the result may coincide, although the branch that makes
-- them coincide is not the one that consumes.
-- ==
-- error: result of if-expression

def main (c: bool) (u: *[]i32) (a: *[]i32) (b: *[]i32) =
  let (r0, r1) = if c then (u, u) else (let _ = u with [0] = 5 in (a, b))
  let r0[0] = 1
  in r1
