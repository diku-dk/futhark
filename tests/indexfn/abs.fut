def abs (x: i64) : {i64 | \y -> y >= 0} =
  if x < 0 then -1 * x else x
