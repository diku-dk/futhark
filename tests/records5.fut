-- Implicit field expressions.
-- ==
-- input { 1 2 } output { 1 2}

let main (x: i32) (y: 32) =
  let r = {y,x}
  in (r.x, r.y)