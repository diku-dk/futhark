-- Ambiguous size of sum type.
-- ==
-- error: ambiguous

type~ sum = #foo ([]i32) | #bar ([]i32)

let main (xs: *[]i32) =
  let v : sum = #foo xs
  in xs
