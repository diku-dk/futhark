-- The simplest conceivable type inference.
-- ==
-- input { 2 } output { 2 }

let id x = x

let main (x: i32) = id x
