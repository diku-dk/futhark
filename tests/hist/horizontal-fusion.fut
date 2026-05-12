--
-- ==
-- input { 2i64 [0, 1, 1] } output { [2, 6] [0f32, 0f32] }
-- structure { Screma 0 Hist 1 }

def main [m] (n: i64) (image: [m]i32) : ([n]i32, []f32) =
  let as = replicate n 0
  let bs = replicate n 0
  in ( reduce_by_index as (+) 0 (map i64.i32 image) (map (+ 2) image)
     , reduce_by_index bs (*) 1 (map i64.i32 image) (map f32.i32 image)
     )
