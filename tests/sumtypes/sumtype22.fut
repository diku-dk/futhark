-- Sumtype aliasing.
-- ==

type^ sum = #foo ([]i32) | #bar ([]i32)

let main (xs: *[]i32) =
  let v : sum = #foo xs
  let xs[0] = 0
  in xs
