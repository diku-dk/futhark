-- ==
-- input { 0f64 1f64 } output { 1f64 0f64 }
-- input { 1f64 1f64 } output { 1f64 1f64 }
-- input { -1f64 1f64 } output { 1f64 -1f64 }
-- input { 1f64 -1f64 } output { 1f64 -1f64 }


let main(x: f64) (y: f64): (f64,f64) =
  (f64.max x y,
   f64.min x y)
