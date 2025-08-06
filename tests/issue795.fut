def main (r_sigma: f32) (I_tiled: [][][]f32) =
  let nz' = i64.f32 (1 / r_sigma + 0.5)
  let bin v = i64.f32 (v / r_sigma + 0.5)
  let intensity cell =
    reduce_by_index (replicate nz' 0)
                    (+)
                    0
                    (cell |> map bin)
                    (map ((* 256) >-> i64.f32) cell)
    |> map (f32.i64 >-> (/ 256))
  let count cell =
    reduce_by_index (replicate nz' 0)
                    (+)
                    0
                    (cell |> map bin)
                    (map (const 1) cell)
  in map2 (map2 zip)
          (map (map intensity) I_tiled)
          (map (map count) I_tiled)
