-- Inference of record projection.
-- ==
-- input { 2 } output { 2 }

def f r =
  let _y = r.l
  in (r : {l: i32})

def main (l: i32) = (f {l}).l
