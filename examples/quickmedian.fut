-- Computing a median in Futhark using a parallel adaptation of
-- Hoare's quickmedian algorithm.  The pivot selection is naive, which
-- may lead to O(n**2) behaviour.
--
-- The implementation is not particularly fast; mostly due to the
-- final iterations that operate on very small arrays, at which point
-- the overhead of parallel execution becomes dominant.  An
-- improvement would be to switch to a sorting-based approach once the
-- input size drops beneath some threshold.
--
-- Oh, and it cannot handle empty inputs.
--
-- ==
-- tags { no_csharp }
-- input { [1] }
-- output { 1 }
-- input { [4, -8, 2, 2, 0, 0, 5, 9, -6, 2] }
-- output { 0 }

let quickmedian [n] (xs: [n]i32): i32 =
  let (_, ys) =
    loop (i, ys : []i32) = (0, xs) while length ys > 1 do
      let pivot = ys[length ys/2]
      let (lt, gte) = partition (<pivot) ys
      in if null lt then (i + 1, tail gte)
         else if i + length lt > n/2 then (i, lt)
         else (i + length lt, gte)
  in ys[0]

let main [n] (xs: [n]i32): i32 = quickmedian xs
