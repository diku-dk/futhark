-- Same function used for two constants.  Inlining must take care not
-- to duplicate names.
-- ==
-- input {}
-- output { 8 }

let f (x: i32) (y: i32) =
  let z = x + y
  in z

let a = f 1 2
let b = f 2 3

let main = a + b
