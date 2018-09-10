--
-- ==
-- structure { Screma 0 GenReduce 1 }

let main [m][n] (hist : *[n]i32, image : [m]i32) : [n]i32 =
  gen_reduce hist (+) 0 image (map (+2) image)
