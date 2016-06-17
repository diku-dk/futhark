-- This test exposed a bug in map-nest creation.  The program involves
-- ISWIM with apparently more complex shapes than the other ISWIM
-- tests.  The bug happened whilst pulling a transpose before the
-- producer.
--
--==
--
-- structure { Map 2 Redomap 1 Scan 1 }

fun []f64 take(int n, []f64 a) = let (first, rest) = split((n), a) in first

fun [num_dates][num_und]f64
correlateDeltas( [num_und][num_und]f64 md_c,
                 [num_dates][num_und]f64 zds
) =
    map( fn [num_und]f64 ([num_und]f64 zi) =>
            map( fn f64 (int j) =>
                    let x = zipWith( *, zi, md_c[j] )
                    in  reduce( +, 0.0, x )
               , iota(num_und) )
       , zds )

fun [num_dates][num_und]f64 blackScholes(
                [num_und][num_und]f64 md_c,
                [num_dates][num_und]f64 md_vols,
                [num_dates][num_und]f64 md_drifts,
                 [num_und]f64            md_starts,
                [num_dates][num_und]f64 bb_arr
           ) =
    let noises = correlateDeltas(md_c, bb_arr) in
        scan( fn []f64 ([]f64 x, []f64 y) => zipWith(*, x, y)
            , md_starts, noises )


fun [][]f64 main(
             [num_und][num_und]f64  md_cs,
             [num_dates][num_und]f64  md_vols,
             [num_dates][num_und]f64  md_drifts,
             [num_und]f64              md_sts,
             [num_dates][num_und]f64  bb_row
) =
  let bd_row = blackScholes(md_cs, md_vols, md_drifts, md_sts, bb_row)
  in  bd_row
