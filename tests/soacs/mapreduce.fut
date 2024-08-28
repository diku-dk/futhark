-- Mapping with a reduction.
--
-- ==
-- tags { no_python }
-- compiled input { 10i64 10i64 }
-- output { [45i64, 145i64, 245i64, 345i64, 445i64, 545i64, 645i64, 745i64, 845i64, 945i64] }
-- compiled input { 5i64 50i64 } auto output
-- structure gpu { SegRed 1 }

def main (n: i64) (m: i64): [n]i64 =
  let a = unflatten (iota (n*m))
  in map (\a_r -> reduce (+) 0 a_r) a
