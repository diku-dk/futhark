-- ==
-- tags { no_csharp }
-- input { 1.0 } output { 1f32 1f64 }
-- input { 4.0 } output { 6f32 6f64 }

let main (x: f64) = (f32.gamma (f32.f64 x),
                     f64.gamma x)
