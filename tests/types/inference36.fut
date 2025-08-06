-- Inference through a record/tuple type.

def f b x (y: (i32, i32)) =
  if x.0 == y.0
  then (x.0, 0)
  else if b then y else x
