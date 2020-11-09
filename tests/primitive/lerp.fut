-- ==
-- input { 0.0 1.0 0.0 } output { 0f32 0f64 }
-- input { 0.0 10.0 0.25 } output { 2.5f32 2.5f64 }
-- input { 0.0 10.0 0.5 } output { 5.0f32 5.0f64 }
-- input { 0.0 10.0 0.75 } output { 7.5f32 7.5f64 }

let main (v0: f64) (v1: f64) (t: f64) =
  (f32.lerp (f32.f64 v0) (f32.f64 v1) (f32.f64 t),
   f64.lerp v0 v1 t)
