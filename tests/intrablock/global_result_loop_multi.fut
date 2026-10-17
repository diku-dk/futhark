-- The shape used by temporal tiling: a loop carrying a ring buffer and
-- two #[scratch]-initialised result accumulators, one plane of each
-- written per iteration.  Each accumulator is constructed directly in
-- its own result; the ring stays in shared memory.  In the second
-- dataset each block's results (2 x 128 KiB) are larger than shared
-- memory.
-- ==
-- random input { [4][8][16]f32 } auto output
-- random input { [4][512][64]f32 } auto output
-- structure gpu-mem { Alloc 3 }

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
