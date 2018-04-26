-- It is OK to bind a name in a let-shape declaration.
--
-- ==
-- input { [1,2,3,4,5,6] }
-- output { 3 }

let even(x: i32): bool = x % 2 == 0

let main [n] (xs: [n]i32): i32 =
  let [num_even] xs': [num_even]i32 = filter even xs
  in num_even
