-- https://rosettacode.org/wiki/Monte_Carlo_methods
--
-- Using Sobol sequences for random numbers.
-- ==
-- tags { no_python }
-- input { 1 } output { 4f32 }
-- input { 10 } output { 3.2f32 }
-- compiled input { 100 } output { 3.16f32 }
-- compiled input { 1000 } output { 3.144f32 }
-- compiled input { 10000 } output { 3.142f32 }
-- compiled input { 100000 } output { 3.14184f32 }
-- compiled input { 1000000 } output { 3.141696f32 }
-- compiled input { 10000000 } output { 3.141595f32 }

def dirvcts () : [2][30]i32 =
  [ [ 536870912
    , 268435456
    , 134217728
    , 67108864
    , 33554432
    , 16777216
    , 8388608
    , 4194304
    , 2097152
    , 1048576
    , 524288
    , 262144
    , 131072
    , 65536
    , 32768
    , 16384
    , 8192
    , 4096
    , 2048
    , 1024
    , 512
    , 256
    , 128
    , 64
    , 32
    , 16
    , 8
    , 4
    , 2
    , 1
    ]
  , [ 536870912
    , 805306368
    , 671088640
    , 1006632960
    , 570425344
    , 855638016
    , 713031680
    , 1069547520
    , 538968064
    , 808452096
    , 673710080
    , 1010565120
    , 572653568
    , 858980352
    , 715816960
    , 1073725440
    , 536879104
    , 805318656
    , 671098880
    , 1006648320
    , 570434048
    , 855651072
    , 713042560
    , 1069563840
    , 538976288
    , 808464432
    , 673720360
    , 1010580540
    , 572662306
    , 858993459
    ]
  ]

def grayCode (x: i32) : i32 = (x >> 1) ^ x

----------------------------------------
--- Sobol Generator
----------------------------------------
def testBit (n: i32, ind: i32) : bool =
  let t = (1 << ind) in (n & t) == t

def xorInds [num_bits] (n: i32) (dir_vs: [num_bits]i32) : i32 =
  let reldv_vals =
    map2 (\dv i ->
            if testBit (grayCode n, i32.i64 i)
            then dv
            else 0)
         dir_vs
         (iota num_bits)
  in reduce (^) 0 reldv_vals

def sobolIndI [m] [num_bits] (dir_vs: [m][num_bits]i32, n: i64) : [m]i32 =
  map (xorInds (i32.i64 n)) dir_vs

def sobolIndR [m] [num_bits] (dir_vs: [m][num_bits]i32) (n: i64) : [m]f32 =
  let divisor = 2.0 ** f32.i64 (num_bits)
  let arri = sobolIndI (dir_vs, n)
  in map (\x -> f32.i32 x / divisor) arri

def main (n: i32) : f32 =
  let rand_nums = map (sobolIndR (dirvcts ())) (iota (i64.i32 n))
  let dists =
    map (\xy ->
           let (x, y) = (xy[0], xy[1]) in f32.sqrt (x * x + y * y))
        rand_nums
  let bs = map (\d -> if d <= 1.0f32 then 1 else 0) dists
  let inside = reduce (+) 0 bs
  in 4.0f32 * f32.i64 (inside) / f32.i32 (n)
