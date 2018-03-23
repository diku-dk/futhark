-- Inference with both overloading and lambdas.
-- ==
-- input { 1 } output { 3 }

let main x = (\y -> x + y) 2
