-- ==
-- random input { [8704]i32 } auto output
-- random input { [14848]i32 } auto output
-- random input { [16384]i32 } auto output
-- random input { [1048576]i32 } auto output
let main input =
  scan (+) 0 input
