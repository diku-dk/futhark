-- The structure test is a bit iffy, as we cannot express a constraint
-- on the ordering.
-- ==
-- input { [[1,2,3]] } output { [[3, 4, 5]] }
-- structure gpu { Index 1 }

def main (xs: [][]i32) = (map (map (+ 2)) xs)[::2]
