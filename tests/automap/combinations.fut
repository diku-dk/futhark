-- All the various ways one can imagine automapping a very simple program.

def plus (x: i32) (y: i32) = x + y

-- ==
-- entry: vecint
-- input { [1,2,3] } output { [3,4,5] }

entry vecint (x: []i32) = plus x 2

-- ==
-- entry: vecvec
-- input { [1,2,3] } output { [2,4,6] }

entry vecvec (x: []i32) = plus x x

-- ==
-- entry: matint
-- input { [[1,2],[3,4]] } output { [[3,4],[5,6]] }

entry matint (x: [][]i32) = plus x 2

-- ==
-- entry: matmat
-- input { [[1,2],[3,4]] } output { [[2,4],[6,8]] }

entry matmat (x: [][]i32) = plus x x

-- ==
-- entry: matvec
-- input { [[1,2],[3,4]] [5,6] } output { [[6,8],[8,10]] }

entry matvec (x: [][]i32) (y: []i32) = plus x y

-- ==
-- entry: vecvecvec
-- input { [1,2,3] } output { [3,6,9] }
entry vecvecvec (x: []i32) = (\x y z -> x + y + z) x x x
