-- Ordering is not defined for sum types.
-- ==
-- error: sum type

let main (x: f32) (y: f32) =
  #foo x > #foo y
