-- Bug in closed-form simplification.
-- ==
-- input { true } output { true }
-- input { false } output { false }
-- structure { Reduce 0 }

def main (x: bool) =
  reduce (&&) true [x]
