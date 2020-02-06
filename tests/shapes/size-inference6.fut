-- Do not permit inference of a type with non-constructive size parameters.
-- ==
-- error: "r"

let r =
  let f = reverse
  let g = reverse
  in {f, g}
