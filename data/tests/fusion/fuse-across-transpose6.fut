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
-- -- md_starts[3]
-- [ 3758.0500000000001819, 11840.0000000000000000, 1200.0000000000000000 ]
-- [	[ 2.2372928847280580, 1.0960951589853829, 0.7075902730592357, 0.8166828043492210, 0.7075902730592357 ],	--bb_sd[5] (standard deviation)
--      [ 0.0000000000000000, 0.5998905309250137, 0.4993160054719562, 0.6669708029197080, 0.5006839945280438 ],	--bb_lw[5]
--      [ 0.0000000000000000, 0.4001094690749863, 0.5006839945280438, 0.3330291970802919, 0.4993160054719562 ]	--bb_rw[5]
-- ]
-- }
-- output {
--   [[5588.085155446795, 15555.466596921435, 1576.3410712135153],
--    [6756.537935152432, 19222.803287709357, 1891.6096055103294],
--    [7596.473396915333, 22764.958002561074, 2196.0263023695247],
--    [8744.595950505505, 28090.879539209436, 2563.082633667557],
--    [9882.132352013321, 33465.152508625804, 2992.2059021836353]]
-- }
-- structure { Map/Scan 1 Map 2 }

fun [f64] take(int n, [f64] a) = let (first, rest) = split((n), a) in first

fun [[f64,num_und],num_dates]
  correlateDeltas([[f64,num_und],num_und  ] md_c,
                  [[f64,num_und],num_dates] zds) =
  map( fn [f64,num_und] ([f64,num_und] zi) =>
         map( fn f64 (int j) =>
                let x = zipWith( *, take(j+1,zi), take(j+1,md_c[j]) )
                in  reduce( +, 0.0, x )
            , iota(num_und) )
     , zds )

fun [f64,num_und] combineVs(  [f64,num_und] n_row,
                               [f64,num_und] vol_row,
                               [f64,num_und] dr_row ) =
  map(+, zip(dr_row, map(*, zip(n_row, vol_row ) )))

fun [[f64,num_und],num_dates]
  mkPrices([f64,num_und]             md_starts,
           [[f64,num_und],num_dates] md_vols,
           [[f64,num_und],num_dates] md_drifts,
           [[f64,num_und],num_dates] noises) =
  let c_rows = map( combineVs, zip(noises, md_vols, md_drifts) )
  let e_rows = map( fn [f64,num_und] ([f64] x) => map(exp64, x)
                  , c_rows
                  )
  in  scan( fn [f64] ([f64] x, [f64] y) => zipWith(*, x, y)
          , md_starts, e_rows )

  -- Formerly blackScholes.
fun [[f64,num_und],num_dates] main([[f64,num_und],num_und  ] md_c,
                                    [[f64,num_und],num_dates] md_vols,
                                    [[f64,num_und],num_dates] md_drifts,
                                    [f64,num_und]            md_starts,
                                    [[f64,num_dates],num_und] bb_arr) =
  -- I don't want to include the entire Brownian bridge, so we just
  -- transpose bb_arr.
  let bb_row = transpose(bb_arr) in
  let noises = correlateDeltas(md_c, bb_row) in
  mkPrices(md_starts, md_vols, md_drifts, noises)
