-- Memory block merging with a concat of the same array twice.  This should
-- coalesce without problems.  However the compiler cannot remove both copy
-- instructions from the final imperative intermediate representation, since
-- either the first part or the second part will need to be inserted by a copy
-- from the other part.
-- ==
-- input { [13, 4] }
-- output { [130, 40, 130, 40] }
-- structure seq-mem { Alloc 1 }
-- structure gpu-mem { Alloc 1 }

def main (xs: []i32) : []i32 =
  let ys = map (* 10) xs
  let zs = concat ys ys
  in zs
