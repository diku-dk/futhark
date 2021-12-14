-- ==
-- compiled input { 10i64 }
-- output {
--   [0.562200927734375f64, 0.482415771484375f64]
--   [0.562200927734375f64, 0.482415771484375f64]
-- }

module x = {
module sobol_dir : {
  val m : [50][8]u32
  val a : [50]u32
  val s : [50]i64
} = {
  def m : [50][8]u32 =
    [[1u32, 0u32, 0u32, 0u32, 0u32, 0u32, 0u32, 0u32],
     [1u32, 3u32, 0u32, 0u32, 0u32, 0u32, 0u32, 0u32],
     [1u32, 3u32, 1u32, 0u32, 0u32, 0u32, 0u32, 0u32],
     [1u32, 1u32, 1u32, 0u32, 0u32, 0u32, 0u32, 0u32],
     [1u32, 1u32, 3u32, 3u32, 0u32, 0u32, 0u32, 0u32],
     [1u32, 3u32, 5u32, 13u32, 0u32, 0u32, 0u32, 0u32],
     [1u32, 1u32, 5u32, 5u32, 17u32, 0u32, 0u32, 0u32],
     [1u32, 1u32, 5u32, 5u32, 5u32, 0u32, 0u32, 0u32],
     [1u32, 1u32, 7u32, 11u32, 19u32, 0u32, 0u32, 0u32],
     [1u32, 1u32, 5u32, 1u32, 1u32, 0u32, 0u32, 0u32],
     [1u32, 1u32, 1u32, 3u32, 11u32, 0u32, 0u32, 0u32],
     [1u32, 3u32, 5u32, 5u32, 31u32, 0u32, 0u32, 0u32],
     [1u32, 3u32, 3u32, 9u32, 7u32, 49u32, 0u32, 0u32],
     [1u32, 1u32, 1u32, 15u32, 21u32, 21u32, 0u32, 0u32],
     [1u32, 3u32, 1u32, 13u32, 27u32, 49u32, 0u32, 0u32],
     [1u32, 1u32, 1u32, 15u32, 7u32, 5u32, 0u32, 0u32],
     [1u32, 3u32, 1u32, 15u32, 13u32, 25u32, 0u32, 0u32],
     [1u32, 1u32, 5u32, 5u32, 19u32, 61u32, 0u32, 0u32],
     [1u32, 3u32, 7u32, 11u32, 23u32, 15u32, 103u32, 0u32],
     [1u32, 3u32, 7u32, 13u32, 13u32, 15u32, 69u32, 0u32],
     [1u32, 1u32, 3u32, 13u32, 7u32, 35u32, 63u32, 0u32],
     [1u32, 3u32, 5u32, 9u32, 1u32, 25u32, 53u32, 0u32],
     [1u32, 3u32, 1u32, 13u32, 9u32, 35u32, 107u32, 0u32],
     [1u32, 3u32, 1u32, 5u32, 27u32, 61u32, 31u32, 0u32],
     [1u32, 1u32, 5u32, 11u32, 19u32, 41u32, 61u32, 0u32],
     [1u32, 3u32, 5u32, 3u32, 3u32, 13u32, 69u32, 0u32],
     [1u32, 1u32, 7u32, 13u32, 1u32, 19u32, 1u32, 0u32],
     [1u32, 3u32, 7u32, 5u32, 13u32, 19u32, 59u32, 0u32],
     [1u32, 1u32, 3u32, 9u32, 25u32, 29u32, 41u32, 0u32],
     [1u32, 3u32, 5u32, 13u32, 23u32, 1u32, 55u32, 0u32],
     [1u32, 3u32, 7u32, 3u32, 13u32, 59u32, 17u32, 0u32],
     [1u32, 3u32, 1u32, 3u32, 5u32, 53u32, 69u32, 0u32],
     [1u32, 1u32, 5u32, 5u32, 23u32, 33u32, 13u32, 0u32],
     [1u32, 1u32, 7u32, 7u32, 1u32, 61u32, 123u32, 0u32],
     [1u32, 1u32, 7u32, 9u32, 13u32, 61u32, 49u32, 0u32],
     [1u32, 3u32, 3u32, 5u32, 3u32, 55u32, 33u32, 0u32],
     [1u32, 3u32, 1u32, 15u32, 31u32, 13u32, 49u32, 245u32],
     [1u32, 3u32, 5u32, 15u32, 31u32, 59u32, 63u32, 97u32],
     [1u32, 3u32, 1u32, 11u32, 11u32, 11u32, 77u32, 249u32],
     [1u32, 3u32, 1u32, 11u32, 27u32, 43u32, 71u32, 9u32],
     [1u32, 1u32, 7u32, 15u32, 21u32, 11u32, 81u32, 45u32],
     [1u32, 3u32, 7u32, 3u32, 25u32, 31u32, 65u32, 79u32],
     [1u32, 3u32, 1u32, 1u32, 19u32, 11u32, 3u32, 205u32],
     [1u32, 1u32, 5u32, 9u32, 19u32, 21u32, 29u32, 157u32],
     [1u32, 3u32, 7u32, 11u32, 1u32, 33u32, 89u32, 185u32],
     [1u32, 3u32, 3u32, 3u32, 15u32, 9u32, 79u32, 71u32],
     [1u32, 3u32, 7u32, 11u32, 15u32, 39u32, 119u32, 27u32],
     [1u32, 1u32, 3u32, 1u32, 11u32, 31u32, 97u32, 225u32],
     [1u32, 1u32, 1u32, 3u32, 23u32, 43u32, 57u32, 177u32],
     [1u32, 3u32, 7u32, 7u32, 17u32, 17u32, 37u32, 71u32]]
  def a : [50]u32 =
    [0u32,
     1u32,
     1u32,
     2u32,
     1u32,
     4u32,
     2u32,
     4u32,
     7u32,
     11u32,
     13u32,
     14u32,
     1u32,
     13u32,
     16u32,
     19u32,
     22u32,
     25u32,
     1u32,
     4u32,
     7u32,
     8u32,
     14u32,
     19u32,
     21u32,
     28u32,
     31u32,
     32u32,
     37u32,
     41u32,
     42u32,
     50u32,
     55u32,
     56u32,
     59u32,
     62u32,
     14u32,
     21u32,
     22u32,
     38u32,
     47u32,
     49u32,
     50u32,
     52u32,
     56u32,
     67u32,
     70u32,
     84u32,
     97u32,
     103u32]
  def s : [50]i64 =
    [1i64,
     2i64,
     3i64,
     3i64,
     4i64,
     4i64,
     5i64,
     5i64,
     5i64,
     5i64,
     5i64,
     5i64,
     6i64,
     6i64,
     6i64,
     6i64,
     6i64,
     6i64,
     7i64,
     7i64,
     7i64,
     7i64,
     7i64,
     7i64,
     7i64,
     7i64,
     7i64,
     7i64,
     7i64,
     7i64,
     7i64,
     7i64,
     7i64,
     7i64,
     7i64,
     7i64,
     8i64,
     8i64,
     8i64,
     8i64,
     8i64,
     8i64,
     8i64,
     8i64,
     8i64,
     8i64,
     8i64,
     8i64,
     8i64,
     8i64]
}

}


module type sobol_dir = {
  val a: [50]u32
  val s: [50]i64
  val m: [50][8]u32
}

module type sobol = {
  val D : i64                                 -- dimensionality of the sobol sequence
  val norm : f64                              -- the value 2**32
  val independent : i64 -> [D]u32             -- [independent i] returns the i'th sobol vector (in u32) representation
  val recurrent : i64 -> [D]u32 -> [D]u32     -- [recurrent i v] returns the i'th sobol vector given v is the (i-1)'th sobol vector
  val chunk : i64 -> (n:i64) -> [n][D]f64     -- [chunk i n] returns the array [v_i,...,v_(i+n-1)] of sobol vectors where v_j is the
  val chunki : i64 -> (n:i64) -> [n][D]u32
  val recM : i64 -> [D]u32
  module Reduce :                             --             j'th D-dimensional sobol vector
      (X : { type t
             val ne	: 	t
             val op	: 	t -> t -> t
             val f : [D]f64 -> t }) -> { val run : i64 -> X.t }
}

module Sobol (DM: sobol_dir) (X: { val D : i64 }) : sobol = {
  def D = X.D

  -- Compute direction vectors. In general, some work can be saved if
  -- we know the number of sobol numbers (N) up front. Here, however,
  -- we calculate sufficiently sized direction vectors to work with
  -- upto N = 2^L, where L=32 (i.e., the maximum number of bits
  -- needed).

  --let L = 32i64
  def L = 16i64

  -- direction vector for dimension j
  def dirvec (j:i64) : [L]u32 =
    if j == 0 then
       map (\i -> 1u32 << (u32.i64(L)-u32.i64(i+1))
           ) (iota L)
    else
       let s = DM.s[j-1]
       let a = DM.a[j-1]
       let V = map (\i -> if i >= s then 0u32
                          else DM.m[j-1,i] << (u32.i64(L)-u32.i64(i+1))
                   ) (iota L)
       let (_,V) = loop (i,V) = (s, V) while i < L do
           let v = V[i-s]
           let vi0 = v ^ (v >> (u32.i64(s)))
           let (_,vi) =
             loop (k,vi) = (1,vi0) while k <= s-1 do
                  (k+1, vi ^ (((a >> u32.i64(s-1-k)) & 1u32) * V[i-k]))
           in (i+1, V with [i] = vi)
       in V

  def index_of_least_significant_0(x: i64): i64 =
    loop i = 0 while i < 64 && ((x>>i)&1) != 0 do i + 1

  def norm = 2.0 f64.** f64.i64(L)

  def grayCode (x: i64): i64 = (x >> 1) ^ x

  def testBit (n: i64) (ind:i64) : bool =
    let t = (1 << ind) in (n & t) == t

  def dirvecs : [D][L]u32 =
    map dirvec (iota D)

  def recSob (i:i64) (dirvec:[L]u32) (x:u32) : u32 =
    x ^ dirvec[index_of_least_significant_0 i]

  def recurrent (i:i64) (xs:[D]u32) : [D]u32 =
    map2 (recSob i) dirvecs xs

  def indSob (n: i64) (dirvec: [L]u32): u32 =
    let reldv_vals = map2 (\dv i -> if testBit (grayCode n) i then dv else 0u32)
                          dirvec (iota L)
    in reduce (^) 0u32 reldv_vals

  def independent (i:i64) : [D]u32 =
    map (indSob i) dirvecs

  -- utils
  def recM (i:i64) : [D]u32 =
    let bit = index_of_least_significant_0 i
    in map (\row -> row[bit]) dirvecs

  -- computes sobol numbers: offs,..,offs+chunk-1
  def chunk (offs:i64) (n:i64) : [n][D]f64 =
    let sob_beg = independent offs
    let contrbs = map (\(k:i64): [D]u32  ->
                       if k==0 then sob_beg
                       else recM (k+offs-1))
                    (iota n)
    let vct_ints = scan (\x y -> map2 (^) x y) (replicate D 0u32) contrbs
    in map (\xs -> map (\x -> f64.u32(x)/norm) xs) vct_ints

  def chunki (offs:i64) (n:i64) : [n][D]u32 =
    let sob_beg = independent offs
    let contrbs = map (\(k:i64): [D]u32  ->
                       if k==0 then sob_beg
                       else recM (k+offs-1))
                    (iota n)
    in scan (\x y -> map2 (^) x y) (replicate D 0u32) contrbs

  module Reduce (X : { type t
                       val ne	: 	t
                       val op	: 	t -> t -> t
                       val f : [D]f64 -> t }) : { val run : i64 -> X.t } =
  {
    def run (N:i64) : X.t =
      reduce_stream_per X.op (\sz (ns:[sz]i64) : X.t ->
                             if sz > 0 then reduce X.op X.ne (map X.f (chunk ns[0] sz))
                             else X.ne)
      (iota N)

  }
}

module S2 = Sobol x.sobol_dir { def D = 2i64 }

def mean [n] (xs: [n]f64) : f64 =
  reduce (+) 0.0 xs / f64.i64(n)

module R = S2.Reduce { type t = i64
                       def ne = 0i64
                       def op (x:i64) (y:i64) = x i64.+ y
                       def f (v : [S2.D]f64) : t =
                         let x = v[0]
                         let y = v[1]
                         in i64.bool(x*x+y*y < 1f64) }

def norm (x:u32) : f64 = f64.u32(x)/S2.norm

def norms [n] (xs:[n]u32) : [n]f64 = map norm xs

def normss [n] [D] (xs:[n][D]u32) : [n][D]f64 = map norms xs

def means [n] [D] (xs:[D][n]f64) : [D]f64 = map mean xs

def main (n: i64) =
  let offs = i64.u32 2323234545
  let a = S2.chunki offs n
  let b = map S2.independent (map (+offs) (iota n))
  in (means (transpose (normss a)), means (transpose (normss b)))
