-- Access a record field inside a module.
-- ==
-- input { 1 } output { 3 }

module m = { let r = { x = 2 } }

let main (x: i32) = m.r.x + x
