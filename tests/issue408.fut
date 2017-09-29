-- Bug in closed-form simplification.
-- ==
-- input { true } { true }
-- input { false } { false }
-- structure { Reduce 0 }

let main (x: bool) =
  reduce (&&) true [x]
