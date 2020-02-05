-- This test exposed a bug in map-nest creation.  The program involves
-- ISWIM with apparently more complex shapes than the other ISWIM
-- tests.  The bug happened whilst pulling a transpose before the
-- producer.
--
--==
--
-- structure { Map 1 Redomap 1 Scanomap 1 }

let take(n: i32, a: []f64): []f64 = let (first, rest) = split (n) a in first

let correlateDeltas [num_und] [num_dates]
                    (md_c:  [num_und][num_und]f64,
                     zds: [num_dates][num_und]f64
                    ): [num_dates][num_und]f64 =
    map (\(zi: [num_und]f64): [num_und]f64  ->
            map (\(j: i32): f64  ->
                    let x = map2 (*) zi (md_c[j] )
                    in  reduce (+) (0.0) x
               ) (iota(num_und) )
       ) zds

let blackScholes [num_und][num_dates]
                (md_c:[num_und][num_und]f64,
                md_vols: [num_dates][num_und]f64,
                md_drifts: [num_dates][num_und]f64,
                md_starts: [num_und]f64,
                bb_arr: [num_dates][num_und]f64
           ): [num_dates][num_und]f64 =
    let noises = correlateDeltas(md_c, bb_arr) in
        scan (\(x: []f64) (y: []f64)  -> map2 (*) x y
            ) (md_starts) noises


let main [num_und][num_dates]
        (md_cs: [num_und][num_und]f64,
         md_vols: [num_dates][num_und]f64,
         md_drifts: [num_dates][num_und]f64,
         md_sts: [num_und]f64,
         bb_row: [num_dates][num_und]f64
        ): [][]f64 =
  let bd_row = blackScholes(md_cs, md_vols, md_drifts, md_sts, bb_row)
  in  bd_row
