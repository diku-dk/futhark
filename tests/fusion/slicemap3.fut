-- This one is not currently fusible.
-- ==
-- input { [[1,2,3],[4,5,6],[7,8,9]] } output { [[3, 5], [9, 11]] }
-- structure gpu { Index 2 }

def main (xs: [][]i32) = (map (map (+2)) xs)[::2,::2]
