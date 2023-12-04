-- ==
-- input { [1i64,2i64,3i64] }
-- output { 6i64 }

def f [n] (reps:[n]i64) : [reduce (+) 0 reps]i64 =
  iota (reduce (+) 0 reps)

entry main arr = length (f (map (\x -> x) arr))
