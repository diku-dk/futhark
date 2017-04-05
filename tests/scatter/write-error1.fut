-- Fail if the index and value arrays do not have the same size.
--
-- ==
-- input { [0] [1] }
-- output { [1,0,0,0,0,0,0,0,0,0] }
-- input { [0] [1,2] }
-- error:

let main(is: [#n]i32, vs: [#m]i32): []i32 =
  let a = replicate 10 0
  in scatter a is vs
