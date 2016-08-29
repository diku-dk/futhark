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

fun grayCode(x: int): int = (x >> 1) ^ x

----------------------------------------
--- Sobol Generator
----------------------------------------
fun testBit(n: int, ind: int): bool =
    let t = (1 << ind) in (n & t) == t

-----------------------------------------------------------------
---- INDEPENDENT FORMULA:
----    filter is redundantly computed inside map.
----    Currently Futhark hoists it outside, but this will
----    not allow fusing the filter with reduce => redomap,
-----------------------------------------------------------------
fun xorInds(n: int) (dir_vs: [num_bits]int): int =
    let reldv_vals = map (fn (dv: int, i: int): int  =>
                            if testBit(grayCode(n),i)
                            then dv else 0
                        ) (zip (dir_vs) (iota(num_bits)) ) in
    reduce (^) 0 (reldv_vals )

fun sobolIndI (dir_vs:  [][]int, n: int ): []int =
    map (xorInds(n)) (dir_vs )

--------------------------------
---- STRENGTH-REDUCED FORMULA
--------------------------------
fun index_of_least_significant_0(num_bits: int, n: int): int =
  let (goon,k) = (True,0) in
  loop ((goon,k,n)) =
        for i < num_bits do
          if(goon)
          then if (n & 1) == 1
               then (True, k+1, n>>1)
               else (False,k,   n   )
          else      (False,k,   n   )
  in k

fun sobolRecI(sob_dir_vs: [][num_bits]int, prev: []int, n: int): []int =
  let bit = index_of_least_significant_0(num_bits,n) in
  map  (fn (vct_prev: ([]int,int)): int  =>
         let (vct_row, prev) = vct_prev in
         vct_row[bit] ^ prev
      ) (zip (sob_dir_vs) prev)

fun recM(sob_dirs:  [][num_bits]int, i: int ): []int =
  let bit= index_of_least_significant_0(num_bits,i) in
  map (fn (row: []int): int => unsafe row[bit]) (sob_dirs )

-- computes sobol numbers: n,..,n+chunk-1
fun sobolChunk(dir_vs: [len][num_bits]int, n: int, chunk: int): [chunk][]f64 =
  let sob_fact= 1.0 / f64(1 << num_bits)
  let sob_beg = sobolIndI(dir_vs, n+1)
  let contrbs = map (fn (k: int): []int  =>
                        let sob = k + n in
                        if(k==0) then sob_beg
                        else recM(dir_vs, k+n)
                   ) (iota(chunk) )
  let vct_ints= scan (fn (x: []int) (y: []int): []int  =>
                        zipWith (^) x y
                    ) (replicate len 0) contrbs in
  map (fn (xs: []int): []f64  =>
             map  (fn (x: int): f64  =>
                     f64(x) * sob_fact
                 ) xs
         ) (vct_ints)

----------------------------------------
-- MAIN
----------------------------------------

fun main(num_mc_it: int,
                  dir_vs_nosz: [][num_bits]int,
                  num_dates: int,
                  num_und: int): [][]f64 =
  let sobvctsz  = num_dates*num_und
  let dir_vs    = reshape (sobvctsz,num_bits) dir_vs_nosz
  let sobol_mat = streamMap (fn (chunk: int) (ns: []int): [][sobvctsz]f64  =>
                                sobolChunk(dir_vs, ns[0], chunk)
                           ) (iota(num_mc_it) ) in

  sobol_mat
