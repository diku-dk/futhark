-- ==
-- compiled random input { [20000]f32 } error: l != 1337

def main [n] (xs: *[n]f32) =
  let xs[1337] = f32.lowest
  let op i j =
    let l = if xs[i] < xs[j] then i else j
    in assert (l != 1337) l
  in scan op 0 (iota n)

-- The following operator fails once partial results reach 100, which for an
-- input of ones happens in the block-wide scans rather than in the per-thread
-- scans.

def op_100 (x: i32) (y: i32) = assert (x + y < 100) (x + y)

-- ==
-- entry: block_nonseg
-- input { 4096i64 } error: x \+ y < 100

entry block_nonseg n = scan op_100 0 (opaque (replicate n 1))

-- ==
-- entry: block_seg
-- input { 4i64 1024i64 } error: x \+ y < 100

entry block_seg m n = map (scan op_100 0) (opaque (replicate m (replicate n 1)))

-- ==
-- entry: block_intra
-- input { 4i64 1024i64 } error: x \+ y < 100

entry block_intra m n =
  #[flattening(only_intra)]
  map (\xs -> scan op_100 0 (map (+ 0) xs)) (opaque (replicate m (replicate n 1)))
