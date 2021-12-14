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
--   49.8203125f32
-- }

def grayCode(x: i32): i32 = (x >> 1) ^ x

----------------------------------------
--/ Sobol Generator
----------------------------------------
def testBit(n: i32, ind: i32): bool =
    let t = (1 << ind) in (n & t) == t

----------------------------------------------------------------/
---- INDEPENDENT FORMULA:
----    filter is redundantly computed inside map.
----    Currently Futhark hoists it outside, but this will
----    not allow fusing the filter with reduce -> redomap,
----------------------------------------------------------------/
def xorInds [num_bits] (n: i32) (dir_vs: [num_bits]i32): i32 =
    let reldv_vals = map (\(dv: i32, i: i32): i32  ->
                            if testBit(grayCode(n),i)
                            then dv else 0
                        ) (zip (dir_vs) (map i32.i64 (iota num_bits))) in
    reduce (^) 0 (reldv_vals )

def sobolIndI [len][num_bits] (dir_vs: [len][num_bits]i32, n: i32 ): [len]i32 =
    map (xorInds(n)) (dir_vs )

def sobolIndR [k][num_bits] (dir_vs: [k][num_bits]i32, n: i32 ): []f32 =
    let divisor = 2.0 ** f32.i64(num_bits)
    let arri    = sobolIndI( dir_vs, n )     in
        map (\x -> f32.i32(x) / divisor) arri

--------------------------------/
---- STRENGTH-REDUCED FORMULA
--------------------------------/

def index_of_least_significant_0(num_bits: i32, n: i32): i32 =
  let (goon,k) = (true,0) in
  (loop ((goon,k,n)) for i < num_bits do
     if(goon)
     then if (n & 1) == 1
          then (true, k+1, n>>1)
          else (false,k,   n   )
     else      (false,k,   n   )).1

def recM [len][num_bits] (sob_dirs:  [len][num_bits]i32, i: i32 ): [len]i32 =
  let bit= index_of_least_significant_0(i32.i64 num_bits,i) in
  map (\(row: []i32): i32 -> row[bit]) (sob_dirs )

def sobolChunk [len][num_bits] (dir_vs: [len][num_bits]i32) (n: i32) (chunk: i64) (sobvctsz: i64): [chunk][len]f32 =
  let sob_fact= 1.0 / f32.i64(1 << num_bits)
  let sob_beg = sobolIndI(dir_vs, n+1)
  let contrbs = map (\(k: i32): [len]i32  ->
                        let sob = k + n in
                        if(k==0) then sobolIndI(dir_vs, n+1)
                        else recM(dir_vs, k+n)
                   ) (map i32.i64 (iota chunk))
  let vct_ints= scan (\x y -> map2 (^) x y) (replicate len 0) contrbs in
  map (\xs: [len]f32  ->
             map  (\(x: i32): f32  ->
                     f32.i32(x) * sob_fact
                 ) xs
         ) vct_ints

def main [k][num_bits]
         (num_dates:  i32) (num_und: i32) (num_mc_it: i32)
         (dir_vs_nosz: [k][num_bits]i32): f32 =
  let sobvctsz  = i64.i32 (num_dates*num_und)
  let dir_vs    = dir_vs_nosz :> [sobvctsz][num_bits]i32
  let sobol_mat = #[sequential_inner]
                  map_stream (\chunk (ns: [chunk]i32): [chunk][sobvctsz]f32 ->
                                sobolChunk dir_vs (if chunk > 0 then ns[0] else 0) chunk sobvctsz
                           ) (map i32.i64 (iota (i64.i32 num_mc_it))) in
  reduce  (+) (0.0) (map  (\(row: []f32): f32  -> reduce (+) (0.0) row) (sobol_mat ) )
