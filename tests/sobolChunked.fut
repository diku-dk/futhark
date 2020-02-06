-- Chunked sobol computation derived from OptionPricing.
--
-- At one point failed in the OpenCL code generator.
--
-- ==
-- input {
-- 10
--
-- [
-- 	[
-- 		536870912, 268435456, 134217728, 67108864, 33554432, 16777216, 8388608, 4194304, 2097152, 1048576, 524288, 262144, 131072, 65536, 32768, 16384, 8192, 4096, 2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1
-- 	]
-- ]
-- 1
-- 1
-- }
-- output { [[0.500000], [0.750000], [0.250000], [0.375000], [0.875000], [0.625000], [0.125000], [0.187500], [0.687500], [0.937500]] }

let grayCode(x: i32): i32 = (x >> 1) ^ x

----------------------------------------
--- Sobol Generator
----------------------------------------
let testBit(n: i32, ind: i32): bool =
    let t = (1 << ind) in (n & t) == t

-----------------------------------------------------------------
---- INDEPENDENT FORMULA:
----    filter is redundantly computed inside map.
----    Currently Futhark hoists it outside, but this will
----    not allow fusing the filter with reduce -> redomap,
-----------------------------------------------------------------
let xorInds [num_bits] (n: i32) (dir_vs: [num_bits]i32): i32 =
    let reldv_vals = map (\(dv: i32, i: i32): i32  ->
                            if testBit(grayCode(n),i)
                            then dv else 0
                        ) (zip (dir_vs) (iota(num_bits)) ) in
    reduce (^) 0 (reldv_vals )

let sobolIndI [len] (dir_vs:  [len][]i32, n: i32 ): [len]i32 =
    map (xorInds(n)) (dir_vs )

--------------------------------
---- STRENGTH-REDUCED FORMULA
--------------------------------
let index_of_least_significant_0(num_bits: i32, n: i32): i32 =
  let (goon,k) = (true,0) in
  let (_,k,_) = loop ((goon,k,n)) for i < num_bits do
    if(goon)
    then if (n & 1) == 1
         then (true, k+1, n>>1)
         else (false,k,   n   )
    else      (false,k,   n   )
  in k

let sobolRecI [len][num_bits] (sob_dir_vs: [len][num_bits]i32, prev: []i32, n: i32): [len]i32 =
  let bit = index_of_least_significant_0(num_bits,n) in
  map  (\(vct_prev: ([]i32,i32)): i32  ->
         let (vct_row, prev) = vct_prev in
         vct_row[bit] ^ prev
      ) (zip (sob_dir_vs) prev)

let recM [len][num_bits] (sob_dirs:  [len][num_bits]i32, i: i32 ): [len]i32 =
  let bit= index_of_least_significant_0(num_bits,i) in
  map (\(row: []i32): i32 -> unsafe row[bit]) (sob_dirs )

-- computes sobol numbers: n,..,n+chunk-1
let sobolChunk [len] [num_bits] (dir_vs: [len][num_bits]i32) (n: i32) (chunk: i32): [chunk][len]f64 =
  let sob_fact= 1.0 / r64(1 << num_bits)
  let sob_beg = sobolIndI(dir_vs, n+1)
  let contrbs = map (\(k: i32) ->
                        let sob = k + n in
                        if(k==0) then sob_beg
                        else recM(dir_vs, k+n)
                   ) (iota(chunk) )
  let vct_ints= scan (\(x: []i32) (y: []i32)  ->
                        map2 (^) x y
                    ) (replicate len 0) contrbs in
  map (\(xs: []i32) ->
             map  (\(x: i32): f64  ->
                     r64(x) * sob_fact
                 ) xs
         ) (vct_ints)

----------------------------------------
-- MAIN
----------------------------------------

let main [num_bits] (num_mc_it: i32)
                    (dir_vs_nosz: [][num_bits]i32)
                    (num_dates: i32)
                    (num_und: i32): [][]f64 =
  let sobvctsz  = num_dates*num_und
  let dir_vs    = dir_vs_nosz :> [sobvctsz][num_bits]i32
  let sobol_mat = map_stream (\chunk (ns: [chunk]i32): [chunk][sobvctsz]f64  ->
                                sobolChunk dir_vs (if chunk > 0 then ns[0] else 0) chunk)
                             (iota num_mc_it)
  in sobol_mat
