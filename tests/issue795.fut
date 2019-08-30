let main (r_sigma: f32) (I_tiled: [][][]f32) =
  let nz' = t32 (1/r_sigma + 0.5)
  let bin v = t32 (v/r_sigma + 0.5)
  let intensity cell =
    reduce_by_index (replicate nz' 0) (+) 0
                    (cell |> map bin)
                    (map ((*256) >-> t32) cell)
    |> map (r32 >-> (/256))
  let count cell =
    reduce_by_index (replicate nz' 0) (+) 0
                    (cell |> map bin)
                    (map (const 1) cell)
  in map2 (map2 zip)
          (map (map intensity) I_tiled)
          (map (map count) I_tiled)
