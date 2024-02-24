-- ==
-- entry: sobolIndR
-- random input { [12][10]i32 i32 }
-- output { true }

-- ==
-- entry: sobolRecI
-- random input { [12][10]i32 [12]i32 i32}
-- output { true }

-- ==
-- entry: sobolReci2
-- random input { [12][10]i32 [12]i32 i32}
-- output { true }

def grayCode(x: i32): i32 = (x >> 1) ^ x

def testBit(n: i32, ind: i32): bool =
    let t = (1 << ind) in (n & t) == t

def xorInds [num_bits] (n: i32) (dir_vs: [num_bits]i32): i32 =
    let reldv_vals = map (\(dv: i32, i): i32  ->
                            if testBit(grayCode(n),i32.i64 i)
                            then dv else 0
                        ) (zip (dir_vs) (iota(num_bits)) ) in
    reduce (^) 0 (reldv_vals )

    
def sobolIndI [len] (dir_vs:  [len][]i32, n: i32 ): [len]i32 =
    map (xorInds(n)) (dir_vs )

def index_of_least_significant_0(num_bits: i32, n: i32): i32 =
  let (goon,k) = (true,0) in
  let (_,k,_) = loop ((goon,k,n)) for i < num_bits do
    if(goon)
    then if (n & 1) == 1
         then (true, k+1, n>>1)
         else (false,k,   n   )
    else      (false,k,   n   )
  in k

def recM [len][num_bits] (sob_dirs:  [len][num_bits]i32, i: i32 ): [len]i32 =
  let bit= index_of_least_significant_0(i32.i64 num_bits,i) in
  map (\(row: []i32): i32 -> row[bit]) (sob_dirs )

def sobolIndR_orig [m][num_bits] (dir_vs: [m][num_bits]i32) (n: i32): [m]f32 =
  let divisor = 2.0 ** f32.i64(num_bits)
  let arri    = map (xorInds n) dir_vs
  in map (\x -> f32.i32(x) / divisor) arri

def sobolRecI_orig [num_bits][n] (sob_dir_vs: [n][num_bits]i32, prev: [n]i32, x: i32): [n]i32 =
  let bit = index_of_least_significant_0(i32.i64 num_bits, x)
  in map2 (\vct_row prev -> vct_row[bit] ^ prev) sob_dir_vs prev

def sobolReci2_orig [n][num_bits] (sob_dirs: [n][num_bits]i32, prev: [n]i32, i: i32): [n]i32=
  let col = recM(sob_dirs, i)
  in map2 (^) prev col

def sobolIndR_am [m][num_bits] (dir_vs: [m][num_bits]i32) (n: i32): [m]f32 =
  let divisor = 2.0 ** f32.i64(num_bits)
  let arri    = xorInds n dir_vs
  in f32.i32 arri / divisor

def sobolRecI_am [num_bits][n] (sob_dir_vs: [n][num_bits]i32, prev: [n]i32, x: i32): [n]i32 =
  let bit = index_of_least_significant_0(i32.i64 num_bits, x)
  in sob_dir_vs[:,bit] ^ prev

def sobolReci2_am [n][num_bits] (sob_dirs: [n][num_bits]i32, prev: [n]i32, i: i32): [n]i32=
  prev ^ recM(sob_dirs, i)

entry sobolIndR [m][num_bits] (dir_vs: [m][num_bits]i32) (n: i32): []bool =
  sobolIndR_orig dir_vs n == sobolIndR_am dir_vs n

entry sobolRecI [num_bits][n] (sob_dir_vs: [n][num_bits]i32) (prev: [n]i32) (x: i32): []bool =
  sobolRecI_orig (sob_dir_vs, prev, x) == sobolRecI_am (sob_dir_vs, prev, x)
                                           
entry sobolReci2 [n][num_bits] (sob_dirs: [n][num_bits]i32) (prev: [n]i32) (i: i32): []bool =
  sobolReci2_orig (sob_dirs, prev, i) == sobolReci2_am (sob_dirs, prev, i)
