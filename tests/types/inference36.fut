-- Inference through a record/tuple type.

let f b x (y: (i32, i32)) =
  if x.1 == y.1 then (x.1, 0)
  else if b then y else x
