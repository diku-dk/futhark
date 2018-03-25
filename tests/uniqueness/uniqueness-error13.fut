-- No cheating uniqueness with tuple shenanigans.
-- ==
-- error: aliased

let main (x: (*[]i32, *[]i32)): (*[]i32, *[]i32) =
  (x.1, x.1)
