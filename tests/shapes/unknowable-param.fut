-- Existential sizes must not be (exclusively) used as a parameter
-- type.
-- ==
-- error: Unknowable size.*imposes constraint

let f (x: bool) =
  let x' = i64.bool x
  in \(_: [x']bool) -> true
