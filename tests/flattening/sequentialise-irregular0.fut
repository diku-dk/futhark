-- The inner parallelism is irregular, but the attribute asks for it to be
-- sequentialised instead of flattened, so no segmented operations should be
-- generated for it.
-- ==
-- random input { [100]i32 } auto output
-- structure gpu { SegMap 1 SegScan 0 SegRed 0 }

def main (ns: []i32) =
  #[flattening(only_inner)]
  #[flattening(sequentialise_irregular)]
  map (\n -> i64.sum (map (* 2) (iota (i64.i32 n & 7)))) ns
