// Hacky quicksort.
fun [int] quicksort([int] xs) =
  let len = size(0,xs) in
  if len < 2
  then xs
  else let pivot = xs[0] in
       let lt = filter (fn bool (int x) => x < pivot, xs) in
       let eq = filter (fn bool (int x) => x == pivot, xs) in
       let gt = filter (fn bool (int x) => pivot < x, xs) in
       concatMap(fn [int] ([int] r) =>
                   if size(0,r) == 0
                   then r else if r[0] == pivot
                               then r else quicksort(r)
                , lt, eq, gt)

fun [int] main([int] xs) = quicksort(xs)
