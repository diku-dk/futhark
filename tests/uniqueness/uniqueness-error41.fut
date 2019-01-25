-- Global variables may not be unique!
-- ==
-- error: constant

let global: *[]i32 = [1,2,3]

let main (x: i32) =
  global with [0] = x
