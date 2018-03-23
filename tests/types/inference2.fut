-- Higher-order inference.
-- ==
-- input { 2 } output { 4 }

let apply f x = f x

let main x = apply (apply (i32.+) x) x
