-- The structure test is a bit iffy, as we cannot express a constraint
-- on the ordering.
-- ==
-- input { [1,2,3] } output { [3,5] }
-- structure gpu { Index 1 }

def main (xs: []i32) = (map (+ 2) xs)[::2]
