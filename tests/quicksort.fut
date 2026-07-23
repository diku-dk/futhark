-- ==
-- tags { disable }
-- input { [1,10,9,8,5,7] }
-- output { [1i32, 5i32, 7i32, 8i32, 9i32, 10i32] }

def quicksort [n] (xs: [n]i32) : [n]i32 =
  if n <= 1
  then xs
  else let pivot = xs[0]
       let [m0][m1][m2] (x0: [m0]i32, x1: [m1]i32, x2: [m2]i32) =
         partition2 (< pivot) (== pivot) xs
       let (_, sorted) =
         flatmap (\k p ->
                    quicksort (sized k (match p
                                        case 0 -> x0
                                        case 1 -> x1
                                        case _ -> x2)))
                 [m0, m1, m2]
                 [0, 1, 2]
       in sized n sorted

entry main = quicksort
