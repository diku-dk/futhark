-- Equality checking of arrays of two dimensions.
-- ==
-- input { [[1,2],[3,4]] [[1,2],[3,4]] }
-- output { true }
-- input { [[1,2],[3,4]] [[1,2],[3,5]] }
-- output { false }

def main (xs: [][]i32) (ys: [][]i32) = xs == ys
