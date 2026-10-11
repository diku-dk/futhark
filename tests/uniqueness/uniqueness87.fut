-- Results of local functions, polymorphic and monomorphic, can be projected,
-- taken apart, and consumed, after which the functions can still be used.
-- ==
-- input { [1, 2, 3] [4, 5, 6] }
-- output { [5, 2, 3] [7, 2, 3] [1, 2, 3] [4, 5, 6] }

def main (xs: []i32) (ys: []i32) =
  let pid 'q (x: q) : q = x
  let mid (x: []i32) : []i32 = x
  let (_, b) = pid (xs, ys)
  let r = pid (copy xs) with [0] = (pid (xs, ys)).1[0] + 1
  let s = mid (copy xs) with [0] = 7
  in (r, s, mid xs, b)
