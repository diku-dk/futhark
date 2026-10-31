-- Intra-block reduction with a noncommutative operator, where the block must
-- be virtualised.  The reduction is then done chunk by chunk, so the chunks
-- had better be visited in order.
-- ==
-- random input { [1][1000]i32 } auto output
-- compiled random input { [10][1000]i32 } auto output
-- compiled random input { [10][257]i32 } auto output
-- structure gpu { SegMap/SegRed 1 }

-- Composition of affine functions; associative but not commutative.
def op (a: (i32, i32)) (b: (i32, i32)) : (i32, i32) =
  (a.0 * b.0, a.1 * b.0 + b.1)

def main xss =
  #[flattening(only_intra)]
  map (\xs ->
         let (a, b) = reduce op (1, 0) (map (\x -> (x % 7 + 1, x % 11)) xs)
         in map (\x -> x + a * 3 + b) xs)
      xss
