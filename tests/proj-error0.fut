--
-- ==
-- error: field

let main(x: (i32,i8,i16)): (i8,i16,i32) =
  (x.1, x.3, x.0)
