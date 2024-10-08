-- Maintain aliases through record updates.
-- ==
-- error: "ys".*consumed
def main (xs: []i32) (ys: *[]i32) =
  let tup = xs (xs, ys) with 0 =
  let ys[0] = 0
  in tup