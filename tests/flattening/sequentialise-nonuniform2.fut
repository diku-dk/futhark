-- Two levels of nonuniform nesting, all sequentialised into the outermost
-- SegMap.
-- ==
-- random input { [50]i32 } auto output
-- structure gpu { SegMap 1 SegScan 0 SegRed 0 }

def main (ns: []i32) =
  #[flattening(only_inner)]
  #[flattening(sequentialise_nonuniform)]
  map (\n ->
         let k = i64.i32 n & 7
         in i64.sum (map (\i -> i64.product (map (+ i) (iota (k - i)))) (iota k)))
      ns
