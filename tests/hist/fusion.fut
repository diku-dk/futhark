--
-- ==
-- structure { Screma 0 Hist 1 }

def main [m] [n] (hist: *[n]i32, image: [m]i32) : [n]i32 =
  reduce_by_index hist (+) 0 (map i64.i32 image) (map (+ 2) image)
