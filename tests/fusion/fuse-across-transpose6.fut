-- Inspired by the blackScholes function in OptionPricing.  This
-- program once malfunctioned because rearrange-pulling did not
-- properly update the lambda indices.
--
-- ==
--
-- input {
-- [
--      [ 1.0000000f32, 0.6000000f32, 0.8000000f32  ],
--      [ 0.6000000f32, 0.8000000f32, 0.1500000f32  ],
--      [ 0.8000000f32, 0.1500000f32, 0.5809475f32  ]
-- ]
-- [
--      [ 0.1900000f32, 0.1900000f32, 0.1500000f32  ],
--      [ 0.1900000f32, 0.1900000f32, 0.1500000f32  ],
--      [ 0.1900000f32, 0.1900000f32, 0.1500000f32  ],
--      [ 0.1900000f32, 0.1900000f32, 0.1500000f32  ],
--      [ 0.1900000f32, 0.1900000f32, 0.1500000f32  ]
-- ]
-- [
--      [ -0.0283491736871803f32, 0.0178771081725381f32, 0.0043096808044729f32  ],
--      [ -0.0183841413744211f32, -0.0044530897672834f32, 0.0024263805987983f32 ],
--      [ -0.0172686581005089f32, 0.0125638544546015f32, 0.0094452810918001f32  ],
--      [ -0.0144179417871814f32, 0.0157411263968213f32, 0.0125315353728014f32  ],
--      [ -0.0121497422218761f32, 0.0182904634062437f32, 0.0151125070556484f32  ]
-- ]
-- [	[ 2.2372928847280580f32, 1.0960951589853829f32, 0.7075902730592357f32, 0.8166828043492210f32, 0.7075902730592357f32 ],
--      [ 0.0000000000000000f32, 0.5998905309250137f32, 0.4993160054719562f32, 0.6669708029197080f32, 0.5006839945280438f32 ],
--      [ 0.0000000000000000f32, 0.4001094690749863f32, 0.5006839945280438f32, 0.3330291970802919f32, 0.4993160054719562f32 ]
-- ]
-- }
-- output {
--   [[1.4869640253447387f32, 1.3138063004156617f32, 1.313617559344596f32],
--    [1.7978839917383833f32, 1.6235475749754527f32, 1.5763413379252744f32],
--    [2.021386995094619f32, 1.9227160475136045f32, 1.8300219186412707f32],
--    [2.326897180853236f32, 2.372540501622419f32, 2.135902194722964f32],
--    [2.6295904397262726f32, 2.8264486916069096f32, 2.4935049184863627f32]]
-- }
-- structure { /Screma 1 /Screma/Screma 1 /Screma/Screma/Screma 1 }

let correlateDeltas [num_und][num_dates]
                   (md_c: [num_und][num_und]f32,
                    zds: [num_dates][num_und]f32): [num_dates][num_und]f32 =
  map (\(zi: [num_und]f32): [num_und]f32  ->
         map (\(j: i32): f32  ->
                let j' = j + 1
                let x = map2 (*) (unsafe take j' zi) (unsafe take j' md_c[j])
                in  reduce (+) (0.0) x
            ) (iota(num_und) )
     ) zds

let combineVs [num_und]
             (n_row:   [num_und]f32,
              vol_row: [num_und]f32,
              dr_row: [num_und]f32 ): [num_und]f32 =
  map2 (+) dr_row (map2 (*) n_row vol_row)

let mkPrices [num_dates][num_und]
            (md_vols: [num_dates][num_und]f32,
             md_drifts: [num_dates][num_und]f32,
             noises: [num_dates][num_und]f32): [num_dates][num_und]f32 =
  let c_rows = map combineVs (zip3 noises (md_vols) (md_drifts) )
  let e_rows = map (\(x: []f32): [num_und]f32  -> map f32.exp x
                  ) (c_rows
                  )
  in  scan (\x y -> map2 (*) x y) (replicate num_und 1.0) (e_rows )

  -- Formerly blackScholes.
let main [num_dates][num_und]
        (md_c: [num_und][num_und]f32)
        (md_vols: [num_dates][num_und]f32)
        (md_drifts: [num_dates][num_und]f32)
        (bb_arr: [num_und][num_dates]f32): [num_dates][num_und]f32 =
  -- I don't want to import the entire Brownian bridge, so we just
  -- transpose bb_arr.
  let bb_row = transpose bb_arr
  let noises = correlateDeltas(md_c, bb_row) in
  mkPrices(md_vols, md_drifts, noises)
