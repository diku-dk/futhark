-- Polymorphic function used with multiple different types.
-- ==
-- input { 1 true 2 } output { 1 true 2 }

let id 't (x: t): t = x

let main (x: i32) (y: bool) (z: i32) = (id x, id y, id z)
