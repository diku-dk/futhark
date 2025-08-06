-- Test a redomap with map-out where each element is also an array.
--
-- ==
-- input { 5i64 2i64 }
-- output { [[0i32, 1i32],
--           [2i32, 3i32],
--           [4i32, 5i32],
--           [6i32, 7i32],
--           [8i32, 9i32]]
--          false
-- }
-- input { 0i64 1i64 }
-- output { empty([0][1]i32) true }

def main (n: i64) (m: i64) : ([][]i32, bool) =
  let ass =
    map (\l : [m]i32 ->
           map i32.i64 (map (+ l * m) (iota (m))))
        (iota (n))
  let ps =
    map2 (\(as: []i32) (i: i32) : bool ->
            as[i] % 2 == 0)
         ass
         (map i32.i64 (map (% m) (iota (n))))
  in (ass, reduce (&&) true ps)
