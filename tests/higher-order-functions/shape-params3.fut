-- A higher-order function that uses the shape parameter as a value term.
-- ==
-- input { [12,17,8,23] } output { [13,18,9,24] 4 }

let map_length [n] (f: i32 -> i32) (xs: [n]i32) : ([n]i32, i32) =
  (map f xs, n)

let main (xs: []i32) = map_length (\(x:i32) -> x+1) xs
