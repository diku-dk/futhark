-- It is OK to bind a name in a let-shape declaration, and to impose
-- in the same declaration.
--
-- ==
-- input { [1,2,3,4,5,6] 3 }
-- output { 6 }

default (f32)

let even(x: i32): bool = x % 2 == 0

let main(xs: [#n]i32, m: i32): i32 =
  let k = n / 2
  let xs': [m][#num_even]i32 = replicate k (filter even xs)
  in num_even + m
