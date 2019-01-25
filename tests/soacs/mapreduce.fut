-- Mapping with a reduction.
--
-- ==
-- tags { no_python }
-- compiled input { 10 10 }
-- output { [45i32, 145i32, 245i32, 345i32, 445i32, 545i32, 645i32, 745i32, 845i32, 945i32] }
-- structure distributed { SegRed 1 }

let main (n: i32) (m: i32): [n]i32 =
  let a = unflatten n m (iota (n*m))
  in map (\a_r -> reduce (+) 0 a_r) a
