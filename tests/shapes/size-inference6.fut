-- Permit inference of a type with non-constructive size parameters.
-- ==
-- input { 0 2 } output { empty([0]i32) [1i32,0i32] }

let r =
  let f = reverse
  let g = reverse
  in {f, g}

let main x y =
  (\p -> (p.f (iota x), p.g (iota y))) r
