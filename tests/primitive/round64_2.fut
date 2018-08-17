-- Rounding floats to whole numbers.
-- Mono C# has issues with this test case
-- ==
-- tags  { no_csharp }
-- input { 0.500000000000001f64 } output { 1f64 }

let main = f64.round
