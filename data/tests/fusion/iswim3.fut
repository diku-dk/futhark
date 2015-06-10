// This test exposed a bug in map-nest creation.  The program involves
// ISWIM with apparently more complex shapes than the other ISWIM
// tests.  The bug happened whilst pulling a transpose before the
// producer.
//
//--
//
// structure { Map 2 Redomap 1 Scan 1 }

fun [real] take(int n, [real] a) = let {first, rest} = split((n), a) in first

fun [[real,num_und],num_dates]
correlateDeltas( [[real,num_und],num_und  ] md_c,
                 [[real,num_und],num_dates] zds
) =
    map( fn [real,num_und] ([real,num_und] zi) =>
            map( fn real (int j) =>
                    let x = zipWith( *, zi, md_c[j] )
                    in  reduce( +, 0.0, x )
               , iota(num_und) )
       , zds )

fun [[real,num_und],num_dates] blackScholes(
                [[real,num_und],num_und  ] md_c,
                [[real,num_und],num_dates] md_vols,
                [[real,num_und],num_dates] md_drifts,
                 [real,num_und]            md_starts,
                [[real,num_und],num_dates] bb_arr
           ) =
    let noises = correlateDeltas(md_c, bb_arr) in
        scan( fn [real] ([real] x, [real] y) => zipWith(*, x, y)
            , md_starts, noises )


fun [[real]] main(
             [[real,num_und],num_und  ]  md_cs,
             [[real,num_und],num_dates]  md_vols,
             [[real,num_und],num_dates]  md_drifts,
             [real,num_und]              md_sts,
             [[real,num_und],num_dates]  bb_row
) =
  let bd_row = blackScholes(md_cs, md_vols, md_drifts, md_sts, bb_row)
  in  bd_row
