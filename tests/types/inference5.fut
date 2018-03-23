-- Inference for a local function.
-- ==
-- input { 2 } output { 4 }

let main x =
  let apply f x = f x
  in apply (apply (i32.+) x) x
