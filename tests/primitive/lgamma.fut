-- ==
-- tags { no_csharp }
-- input { 1.0 } output { 0f32 0f64 }
-- input { 4.0 } output { 1.7917594692280554f32 1.7917594692280554f64 }

let main (x: f64) = (f32.lgamma (f32.f64 x),
                     f64.lgamma x)
