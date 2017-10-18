-- It is OK to bind a size in a loop.
--
-- ==
-- input { [1,2,3,4,5,6] }
-- output { 3 [0,1,2] }

default (f32)

let even(x: i32): bool = x % 2 == 0

let main [n] (xs: [n]i32): (i32, []i32) =
  let [num_even] (evens: [num_even]i32) =
    loop [num_even] (evens: [num_even]i32) = replicate 0 0 for i < n do
      (if even xs[i] then concat evens [num_even] else evens)
  in (num_even, evens)
