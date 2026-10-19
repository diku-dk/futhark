-- A per-block result built by a loop that also carries a block-local
-- ring buffer.  Only the result moves to global memory; the ring stays
-- in shared memory.  In the second dataset each block's result
-- (128 KiB) is larger than shared memory.
-- ==
-- random input { [4][8][16]f32 } auto output
-- random input { [4][512][64]f32 } auto output
-- structure gpu-mem { Alloc 2 }

entry main [m] [nz] [n] (a: [m][nz][n]f32) =
  #[flattening(only_intra)]
  #[intrablock_result(global)]
  map (\col ->
    let (_, oe) =
      loop (ring, oe) = (replicate 2 (replicate n 0f32),
                         #[scratch] replicate nz (replicate n 0f32))
      for p < nz do
        let cur = map2 (+) col[p] ring[p & 1]
        let ring = ring with [p & 1] = cur
        let oe = oe with [p] = map (2*) cur
        in (ring, oe)
    in oe) a
