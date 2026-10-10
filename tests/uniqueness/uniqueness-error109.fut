-- A loop may permute its observed parameters, so a component of its result may
-- be an observed parameter of the function.
-- ==
-- error: Consuming "xs", which is not consumable

def main (xs: []i32) (ys: []i32) =
  let (a, _) = loop (a, b) = (copy ys, xs) for _i < 1 do (b, a)
  let a[0] = 42
  in (a, xs)
