-- It is allowed to consume the same array in different branches of a
-- pattern match (similar to 'if'-expressions).
-- ==
-- input { [1,2,3] true } output { [0,2,3] }
-- input { [1,2,3] false } output { [1,1,3] }

let main (xs: *[]i32) (b: bool) =
  match b
  case true -> xs with [0] = 0
  case false -> xs with [1] = 1
