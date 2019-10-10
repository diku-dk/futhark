--
-- ==
-- input { 2 [0, 1, 1] } output { [2, 6] [0f32, 0f32] }
-- structure { Screma 0 Hist 1 }

let main [m] (n: i32) (image : [m]i32) : ([n]i32, []f32) =
  let as = replicate n 0
  let bs = replicate n 0
  in (reduce_by_index as (+) 0 image (map (+2) image),
      reduce_by_index bs (*) 1 image (map r32 image))
