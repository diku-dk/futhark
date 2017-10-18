-- Simplest polymorphic function.
-- ==
-- input { 1 } output { 1 }

let id 't (x: t): t = x

let main(x: i32) = id x
