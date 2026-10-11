-- A loop may move values between its observed parameters from one iteration to
-- the next, so a component of its result may be anything the body returns for
-- an observed parameter.
-- ==
-- error: Consuming "ys", which is not consumable

def main (xs: []i32) (ys: []i32) =
  let (_, b) = loop (a, b) = (copy xs, copy xs) for _i < 2 do (ys, a)
  let b[0] = 1
  in (b, ys)
