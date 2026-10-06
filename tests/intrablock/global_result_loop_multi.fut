-- The shape used by temporal tiling: a loop carrying a ring buffer and
-- several result accumulators (one plane written per iteration), with
-- the block returning all of them.  Each result accumulator must alias
-- its own slice of its own global result; the ring stays in shared
-- memory.  The second dataset makes each block's results (2 x 128 KiB)
-- larger than shared memory, so staging them there fails.
-- ==
-- random input { [4][8][16]f32 } auto output
-- random input { [4][512][64]f32 } auto output

entry main [m] [nz] [n] (a: [m][nz][n]f32) =
  #[flattening(only_intra)]
  #[intrablock_result(global)]
  map (\col ->
    let (_, oe, oh) =
      loop (ring, oe, oh) = (replicate 2 (replicate n 0f32),
                             #[scratch] replicate nz (replicate n 0f32),
                             #[scratch] replicate nz (replicate n 0f32))
      for p < nz do
        let cur = map2 (+) col[p] ring[p & 1]
        let ring = ring with [p & 1] = cur
        let oe = oe with [p] = map (2*) cur
        let oh = oh with [p] = map (3*) cur
        in (ring, oe, oh)
    in (oe, oh)) a
  |> unzip
