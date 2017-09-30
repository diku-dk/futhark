-- Bug in closed-form simplification.
-- ==
-- input { true } output { true }
-- input { false } output { false }
-- structure { Reduce 0 }

let main (x: bool) =
  reduce (&&) true [x]
