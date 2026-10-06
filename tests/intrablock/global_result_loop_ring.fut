-- An intra-block result built by a loop that also carries other
-- (block-local) accumulators, here a two-plane ring buffer.  Only the
-- result accumulator should be placed in global memory; the ring stays
-- in shared memory.  The second dataset makes each block's result
-- (128 KiB) larger than shared memory, so staging it there fails.
-- ==
-- random input { [4][8][16]f32 } auto output
-- random input { [4][512][64]f32 } auto output

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
