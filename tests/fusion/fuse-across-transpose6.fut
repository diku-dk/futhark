-- Inspired by the blackScholes function in OptionPricing.  This
-- program once malfunctioned because rearrange-pulling did not
-- properly update the lambda indices.
--
-- ==
--
-- input {
-- [     -- md_c[3][3] (begin)
--      [ 1.0000000 , 0.6000000 , 0.8000000  ],
--      [ 0.6000000 , 0.8000000 , 0.1500000  ],
--      [ 0.8000000 , 0.1500000 , 0.5809475  ]
-- ]
-- [     -- md_vols[5][3] volatility (begin)
--      [ 0.1900000 , 0.1900000 , 0.1500000  ],
--      [ 0.1900000 , 0.1900000 , 0.1500000  ],
--      [ 0.1900000 , 0.1900000 , 0.1500000  ],
--      [ 0.1900000 , 0.1900000 , 0.1500000  ],
--      [ 0.1900000 , 0.1900000 , 0.1500000  ]
-- ]
-- [     -- md_drifts[5][3] (begin)
--      [ -0.0283491736871803 , 0.0178771081725381 , 0.0043096808044729  ],
--      [ -0.0183841413744211 , -0.0044530897672834 , 0.0024263805987983 ],
--      [ -0.0172686581005089 , 0.0125638544546015 , 0.0094452810918001  ],
--      [ -0.0144179417871814 , 0.0157411263968213 , 0.0125315353728014  ],
--      [ -0.0121497422218761 , 0.0182904634062437 , 0.0151125070556484  ]
-- ]
-- [	[ 2.2372928847280580, 1.0960951589853829, 0.7075902730592357, 0.8166828043492210, 0.7075902730592357 ],	--bb_sd[5] (standard deviation)
--      [ 0.0000000000000000, 0.5998905309250137, 0.4993160054719562, 0.6669708029197080, 0.5006839945280438 ],	--bb_lw[5]
--      [ 0.0000000000000000, 0.4001094690749863, 0.5006839945280438, 0.3330291970802919, 0.4993160054719562 ]	--bb_rw[5]
-- ]
-- }
-- output {
--   [[1.4869640253447387f64, 1.3138063004156617f64, 1.313617559344596f64],
--    [1.7978839917383833f64, 1.6235475749754527f64, 1.5763413379252744f64],
--    [2.021386995094619f64, 1.9227160475136045f64, 1.8300219186412707f64],
--    [2.326897180853236f64, 2.372540501622419f64, 2.135902194722964f64],
--    [2.6295904397262726f64, 2.8264486916069096f64, 2.4935049184863627f64]]
-- }
-- structure { /Screma 1 /Screma/Screma 1 /Screma/Screma/Screma 1 }

import "/futlib/math"

let correlateDeltas [num_und][num_dates]
                   (md_c: [num_und][num_und]f64,
                    zds: [num_dates][num_und]f64): [num_dates][num_und]f64 =
  map (\(zi: [num_und]f64): [num_und]f64  ->
         map (\(j: i32): f64  ->
                let x = map2 (*) (unsafe take (j+1) zi) (unsafe take (j+1) md_c[j])
                in  reduce (+) (0.0) x
            ) (iota(num_und) )
     ) zds

let combineVs [num_und]
             (n_row:   [num_und]f64,
              vol_row: [num_und]f64,
              dr_row: [num_und]f64 ): [num_und]f64 =
  map2 (+) dr_row (map2 (*) n_row vol_row)

let mkPrices [num_dates][num_und]
            (md_vols: [num_dates][num_und]f64,
             md_drifts: [num_dates][num_und]f64,
             noises: [num_dates][num_und]f64): [num_dates][num_und]f64 =
  let c_rows = map combineVs (zip noises (md_vols) (md_drifts) )
  let e_rows = map (\(x: []f64): [num_und]f64  -> map f64.exp x
                  ) (c_rows
                  )
  in  scan (\(x: []f64) (y: []f64): []f64  -> map2 (*) x y
          ) (replicate num_und 1.0) (e_rows )

  -- Formerly blackScholes.
let main [num_dates][num_und]
        (md_c: [num_und][num_und]f64,
         md_vols: [num_dates][num_und]f64,
         md_drifts: [num_dates][num_und]f64,
         bb_arr: [num_und][num_dates]f64): [num_dates][num_und]f64 =
  -- I don't want to import the entire Brownian bridge, so we just
  -- transpose bb_arr.
  let bb_row = transpose bb_arr
  let noises = correlateDeltas(md_c, bb_row) in
  mkPrices(md_vols, md_drifts, noises)
