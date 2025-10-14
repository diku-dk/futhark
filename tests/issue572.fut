-- The issue was applying tiling inside a loop that does not run the
-- same number of iterations for each thread in a workgroup.
-- ==
-- input { [1,2,3,4,5] }
-- output { 150 }

def main (xs: []i32) =
  reduce (+) 0 (map (\x -> reduce (+) 0 (map (+ x) xs)) xs)
