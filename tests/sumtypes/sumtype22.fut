-- Sumtype aliasing.
-- ==

type sum = #foo ([3]i32) | #bar ([2]i32)

let main (xs: *[3]i32) =
  let v : sum = #foo xs
  let xs[0] = 0
  in xs
