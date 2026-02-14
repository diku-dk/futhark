-- Validation of flattening with 2 lists
--
-- ==
-- entry: validate_flattening_2
-- input {[0i64, 1i64, 2i64, 3i64, 4i64, 5i64]
--        [0i64, 2i64, 4i64, 6i64, 8i64, 10i64]
--        [0i64,0i64,2i64,6i64,12i64,20i64,30i64]
--        [0i64,0i64,
--         0i64,1i64,0i64,1i64,
--         0i64,1i64,2i64,0i64,1i64,4i64,
--         0i64,1i64,2i64,3i64,0i64,1i64,4i64,9i64,
--         0i64,1i64,2i64,3i64,4i64,0i64,1i64,4i64,9i64,16i64
--]}
-- output {[true, true, true, true, true, true]}
entry validate_flattening_2 (ns: []i64) (shp: []i64) (offsets: []i64) (expected: []i64) : []bool =
    map2 (\n i ->
      let irreg = opaque (iota n `concat` (iota n |> map (**2)))
      in
      if shp[i] == 0i64 && length irreg == 0i64 then true
      else if shp[i] == 0i64 then false
      else
        let gts = iota shp[i] |> map (\j -> expected[offsets[i] + j]) :> [n + n]i64
        let pairs = zip irreg gts
        let eq = map (\(pd, gt) -> pd == gt) pairs
        in
          reduce (&&) true eq
    )ns (indices ns)
