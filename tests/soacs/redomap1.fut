-- Test a redomap with map-out where each element is also an array.
--
-- ==
-- input { 5 2 }
-- output { [[0i32, 1i32],
--           [2i32, 3i32],
--           [4i32, 5i32],
--           [6i32, 7i32],
--           [8i32, 9i32]]
--          false
-- }
-- input { 0 0 }
-- output { empty([]i32) true }

fun main(n: i32, m: i32): ([][]i32, bool) =
  let ass = map  (\(l: i32): [m]i32  ->
                   map (+l*m) (iota(m))) (
                 iota(n))
  let ps = map (\(as: []i32) (i: i32): bool  ->
                     unsafe as[i] % 2 == 0) ass (map (%m) (iota(n)))
  in (ass, reduce (&&) true ps)
