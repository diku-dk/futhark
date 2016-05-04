-- This test exposed a bug in map-nest creation.  The program involves
-- ISWIM with apparently more complex shapes than the other ISWIM
-- tests.  The bug happened whilst pulling a transpose before the
-- producer.
--
--==
--
-- structure { Map 2 Redomap 1 Scan 1 }

fun [f64] take(int n, [f64] a) = let (first, rest) = split((n), a) in first

fun [[f64,num_und],num_dates]
correlateDeltas( [[f64,num_und],num_und  ] md_c,
                 [[f64,num_und],num_dates] zds
) =
    map( fn [f64,num_und] ([f64,num_und] zi) =>
            map( fn f64 (int j) =>
                    let x = zipWith( *, zi, md_c[j] )
                    in  reduce( +, 0.0, x )
               , iota(num_und) )
       , zds )

fun [[f64,num_und],num_dates] blackScholes(
                [[f64,num_und],num_und  ] md_c,
                [[f64,num_und],num_dates] md_vols,
                [[f64,num_und],num_dates] md_drifts,
                 [f64,num_und]            md_starts,
                [[f64,num_und],num_dates] bb_arr
           ) =
    let noises = correlateDeltas(md_c, bb_arr) in
        scan( fn [f64] ([f64] x, [f64] y) => zipWith(*, x, y)
            , md_starts, noises )


fun [[f64]] main(
             [[f64,num_und],num_und  ]  md_cs,
             [[f64,num_und],num_dates]  md_vols,
             [[f64,num_und],num_dates]  md_drifts,
             [f64,num_und]              md_sts,
             [[f64,num_und],num_dates]  bb_row
) =
  let bd_row = blackScholes(md_cs, md_vols, md_drifts, md_sts, bb_row)
  in  bd_row
