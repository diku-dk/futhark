-- Inference of record projection.
-- ==
-- input { 2 } output { 2 }

let f r = let _y = r.l
          in (r: {l: i32})

let main (l: i32) = (f {l}).l
