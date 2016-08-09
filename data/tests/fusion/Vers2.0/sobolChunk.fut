-- ==
-- input {
--   1
--   1
--   100
--   [ [ 536870912, 268435456, 134217728, 67108864, 33554432, 16777216, 8388608, 4194304, 2097152, 1048576
--     , 524288,    262144,    131072,    65536,    32768,    16384,    8192,    4096,    2048,    1024
--     , 512,       256,       128,       64,       32,       16,       8,       4,       2,       1
--     ]
--   ]
-- }
-- output {
--   49.8203125
-- }


fun int grayCode(int x) = (x >> 1) ^ x

----------------------------------------
--/ Sobol Generator
----------------------------------------
fun bool testBit(int n, int ind) =
    let t = (1 << ind) in (n & t) == t

----------------------------------------------------------------/
---- INDEPENDENT FORMULA:
----    filter is redundantly computed inside map.
----    Currently Futhark hoists it outside, but this will
----    not allow fusing the filter with reduce => redomap,
----------------------------------------------------------------/
fun int xorInds(int n, [num_bits]int dir_vs) =
    let reldv_vals = map( fn int (int dv, int i) =>
                            if testBit(grayCode(n),i)
                            then dv else 0
                        , zip(dir_vs,iota(num_bits)) ) in
    reduce( ^, 0, reldv_vals )

fun []int sobolIndI ( [][]int dir_vs, int n ) =
    map( xorInds(n), dir_vs )

fun []f64 sobolIndR( [][num_bits]int dir_vs, int n ) =
    let divisor = 2.0 ** f64(num_bits) in
    let arri    = sobolIndI( dir_vs, n )     in
        map( fn f64 (int x) => f64(x) / divisor, arri )

--------------------------------/
---- STRENGTH-REDUCED FORMULA
--------------------------------/

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

fun [len]int recM( [len][num_bits]int sob_dirs, int i ) =
  let bit= index_of_least_significant_0(num_bits,i) in
  map( fn int([]int row) => unsafe row[bit], sob_dirs )

fun [chunk][sobvctsz]f64 sobolChunk([len][num_bits]int dir_vs, int n, int chunk, int sobvctsz) =
  let sob_fact= 1.0 / f64(1 << num_bits)       in
  let sob_beg = sobolIndI(dir_vs, n+1)             in
  let contrbs = map( fn [len]int (int k) =>
                        let sob = k + n in
                        if(k==0) then sobolIndI(dir_vs, n+1)
                        else recM(dir_vs, k+n)
                   , iota(chunk) )                 in
  let vct_ints= scan( fn []int ([]int x, []int y) =>
                        zipWith(^, x, y)
                    , replicate(len, 0), contrbs ) in
  map( fn [len]f64 ([]int xs) =>
             map ( fn f64 (int x) =>
                     f64(x) * sob_fact
                 , xs)
         , vct_ints)

fun f64 main( int num_dates, int num_und, int num_mc_it,
               [][num_bits]int dir_vs_nosz ) =
  let sobvctsz  = num_dates*num_und in
  let dir_vs    = reshape( (sobvctsz,num_bits), dir_vs_nosz ) in
--  let sobol_mat = sobolChunk( dir_vs, 0, num_mc_it ) in
  let sobol_mat = streamMap( fn [][sobvctsz]f64 (int chunk, []int ns) =>
                                sobolChunk(dir_vs, ns[0], chunk, sobvctsz)
                           , iota(num_mc_it) ) in
  reduce (+, 0.0, map ( fn f64 ([]f64 row) => reduce(+, 0.0, row), sobol_mat ) )
