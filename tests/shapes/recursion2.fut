-- A recursive function without a declared return type may return a different
-- size from each call, also when one occurrence is called repeatedly.
-- ==
-- input { [3, 1, 4, 1, 5, 9, 2, 6] } output { [1, 3] }

def h [n] (xs: [n]i32) =
  if n <= 1
  then xs
  else let f i = if i == 0 then h (filter (< xs[0]) xs) else [xs[0]]
       let (_, _, _, ys) = flatmap' f [0, 1]
       in ys

entry main (xs: []i32) = h xs
