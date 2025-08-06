-- ==
-- input { 2i64 [1,2,3,4,5,6,7,8,9] }
-- output { [[1i32, 2i32, 3i32], [3i32, 4i32, 5i32]] }
-- structure gpu { SegMap 1 }

def main (n: i64) (xs: []i32) =
  tabulate n (\i ->
                let ys = #[unsafe] xs[i:i + 3] :> [3]i32
                in map (+ i32.i64 i) ys)
