-- It is OK to bind a size in a loop declaration, and to impose
-- in the same declaration.
--
-- ==
-- input { [1,2,3,4,5,6] 3 }
-- output { 6 }

default (f32)

let even(x: i32): bool = x % 2 == 0

let main(xs: [#n]i32, m: i32): i32 =
  let k = n / 2
  let (evens: [#num_even][m]i32) =
    loop ((evens: [#num_even][m]i32) = replicate 0 (replicate k 0)) for i < n do
    (if even xs[i] then concat evens [replicate k xs[i]] else evens)
  in num_even + m
