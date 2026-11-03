-- A variant conditional whose branches produce a regular multi-dimensional
-- result. This exercises flattening of guarded Match branches with a non-scalar
-- row type, namely that blanks synthesised for an untaken branch must preserve
-- the element type (only the sizes are zeroed), so that merging the branch
-- results by scatter is well-typed.
-- ==
-- input { [true, false, true] [[1,2,3],[4,5,6],[7,8,9]] }
-- output { [[2,3,4],[8,10,12],[8,9,10]] }
-- input { empty([0]bool) empty([0][3]i32) }
-- output { empty([0][3]i32) }

def main [n] (cs: [n]bool) (xss: [n][3]i32) : [n][3]i32 =
  map2 (\c xs -> if c then map (+ 1) xs else map (* 2) xs) cs xss
