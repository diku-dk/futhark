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

fun int grayCode(int x) = (x >> 1) ^ x

----------------------------------------
--- Sobol Generator
----------------------------------------
fun bool testBit(int n, int ind) =
    let t = (1 << ind) in (n & t) == t

-----------------------------------------------------------------
---- INDEPENDENT FORMULA:
----    filter is redundantly computed inside map.
----    Currently Futhark hoists it outside, but this will
----    not allow fusing the filter with reduce => redomap,
-----------------------------------------------------------------
fun int xorInds(int n, [int,num_bits] dir_vs) =
    let reldv_vals = map( fn int (int dv, int i) =>
                            if testBit(grayCode(n),i)
                            then dv else 0
                        , zip(dir_vs,iota(num_bits)) ) in
    reduce( ^, 0, reldv_vals )

fun [int] sobolIndI ( [[int]] dir_vs, int n ) =
    map( xorInds(n), dir_vs )

--------------------------------
---- STRENGTH-REDUCED FORMULA
--------------------------------
fun int index_of_least_significant_0(int num_bits, int n) =
  let (goon,k) = (True,0) in
  loop ((goon,k,n)) =
        for i < num_bits do
          if(goon)
          then if (n & 1) == 1
               then (True, k+1, n>>1)
               else (False,k,   n   )
          else      (False,k,   n   )
  in k

fun [int] sobolRecI([[int,num_bits]] sob_dir_vs, [int] prev, int n) =
  let bit = index_of_least_significant_0(num_bits,n) in
  map (fn int (([int],int) vct_prev) =>
         let (vct_row, prev) = vct_prev in
         vct_row[bit] ^ prev
      , zip(sob_dir_vs,prev))

fun [int] recM( [[int,num_bits]] sob_dirs, int i ) =
  let bit= index_of_least_significant_0(num_bits,i) in
  map( fn int([int] row) => unsafe row[bit], sob_dirs )

-- computes sobol numbers: n,..,n+chunk-1
fun [[f64],chunk] sobolChunk([[int,num_bits],len] dir_vs, int n, int chunk) =
  let sob_fact= 1.0 / f64(1 << num_bits)       in
  let sob_beg = sobolIndI(dir_vs, n+1)             in
  let contrbs = map( fn [int] (int k) =>
                        let sob = k + n in
                        if(k==0) then sob_beg
                        else recM(dir_vs, k+n)
                   , iota(chunk) )                 in
  let vct_ints= scan( fn [int] ([int] x, [int] y) =>
                        zipWith(^, x, y)
                    , replicate(len, 0), contrbs ) in
  map( fn [f64] ([int] xs) =>
             map ( fn f64 (int x) =>
                     f64(x) * sob_fact
                 , xs)
         , vct_ints)

----------------------------------------
-- MAIN
----------------------------------------

fun [[f64]] main(int num_mc_it,
                  [[int,num_bits]] dir_vs_nosz,
                  int num_dates,
                  int num_und) =
  let sobvctsz  = num_dates*num_und in
  let dir_vs    = reshape( (sobvctsz,num_bits), dir_vs_nosz ) in
  let sobol_mat = streamMap( fn [[f64,sobvctsz]] (int chunk, [int] ns) =>
                                sobolChunk(dir_vs, ns[0], chunk)
                           , iota(num_mc_it) ) in

  sobol_mat
