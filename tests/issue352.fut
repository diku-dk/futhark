-- ==
-- compiled input { 10000 } output { 3.14 }

module x = {
module sobol_dir : {
  val m : [50][8]u32
  val a : [50]u32
  val s : [50]i32
} = {
  let m : [50][8]u32 =
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
  let a : [50]u32 =
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
  let s : [50]i32 =
    [1i32,
     2i32,
     3i32,
     3i32,
     4i32,
     4i32,
     5i32,
     5i32,
     5i32,
     5i32,
     5i32,
     5i32,
     6i32,
     6i32,
     6i32,
     6i32,
     6i32,
     6i32,
     7i32,
     7i32,
     7i32,
     7i32,
     7i32,
     7i32,
     7i32,
     7i32,
     7i32,
     7i32,
     7i32,
     7i32,
     7i32,
     7i32,
     7i32,
     7i32,
     7i32,
     7i32,
     8i32,
     8i32,
     8i32,
     8i32,
     8i32,
     8i32,
     8i32,
     8i32,
     8i32,
     8i32,
     8i32,
     8i32,
     8i32,
     8i32]
}

}



module type sobol_dir = {
  val a: [50]u32
  val s: [50]i32
  val m: [50][8]u32
}

module type sobol = {
  val D : i32                                 -- dimensionality of the sobol sequence
  val norm : f64                              -- the value 2**32
  val independent : i32 -> [D]u32             -- [independent i] returns the i'th sobol vector (in u32) representation
  val recurrent : i32 -> [D]u32 -> [D]u32     -- [recurrent i v] returns the i'th sobol vector given v is the (i-1)'th sobol vector
  val chunk : i32 -> (n:i32) -> [n][D]f64     -- [chunk i n] returns the array [v_i,...,v_(i+n-1)] of sobol vectors where v_j is the
  module Reduce :                             --             j'th D-dimensional sobol vector
      (X : { type t
             val ne	: 	t
             val op	: 	t -> t -> t
             val f : [D]f64 -> t }) -> { val run : i32 -> X.t }
}

module Sobol (DM: sobol_dir) (X: { val D : i32 }) : sobol = {
  let D = X.D

  -- Compute direction vectors. In general, some work can be saved if
  -- we know the number of sobol numbers (N) up front. Here, however,
  -- we calculate sufficiently sized direction vectors to work with
  -- upto N = 2^L, where L=32 (i.e., the maximum number of bits
  -- needed).

  let L = 32i32

  -- direction vector for dimension j
  let dirvec (j:i32) : [L]u32 = unsafe
    if j == 0 then
       map (\i -> 1u32 << (32u32-u32.i32(i+1))
 	   ) (iota L)
    else
       let s = DM.s[j-1]
       let a = DM.a[j-1]
       let V = map (\i -> if i >= s then 0u32
			  else DM.m[j-1,i] << (32u32-u32.i32(i+1))
		   ) (iota L) in
       (loop (i,V : *[L]u32) = (s, V) while i < L do
          let v = V[i-s]
	  let vi0 = v ^ (v >> (u32.i32(s)))
	  let (_,vi) =
	    loop (k,vi) = (1,vi0) while k <= s-1 do
              (k+1, vi ^ (((a >> u32.i32(s-1-k)) & 1u32) * V[i-k]))
	  in (i+1, V with [i] = vi)).1

  let index_of_least_significant_0(x: i32): i32 =
    loop i = 0 while i < 32 && ((x>>i)&1) != 0 do i + 1

  let norm = 2.0 f64.** 32.0

  let grayCode (x: i32): i32 = (x >> 1) ^ x

  let testBit (n: i32) (ind:i32) : bool =
    let t = (1 << ind) in (n & t) == t

  let dirvecs : [D][L]u32 =
    map dirvec (iota D)

  let recSob (i:i32) (dirvec:[L]u32) (x:u32) : u32 =
    unsafe if i == 0 then 0u32 else x ^ dirvec[index_of_least_significant_0 (i-1)]

  let recurrent (i:i32) (xs:[D]u32) : [D]u32 =
    map2 (recSob i) dirvecs xs

  let indSob (n: i32) (dirvec: [L]u32): u32 =
    let reldv_vals = map2 (\dv i -> if testBit (grayCode n) i then dv else 0u32)
                         dirvec (iota L)
    in reduce (^) 0u32 reldv_vals

  let independent (i:i32) : [D]u32 =
    map (indSob i) dirvecs

  -- utils
  let recM (i:i32) : [D]u32 =
    let bit = index_of_least_significant_0 i
    in map (\row -> unsafe row[bit]) dirvecs

  -- computes sobol numbers: offs,..,offs+chunk-1
  let chunk (offs:i32) (n:i32) : [n][D]f64 =
    let sob_beg = independent offs
    let contrbs = map (\(k:i32): [D]u32  ->
                       if k==0 then sob_beg
                       else recM (k+offs-1))
                    (iota n)
    let vct_ints = scan (\x y -> map2 (^) x y) (replicate D 0u32) contrbs
    in map (\xs -> map (\x -> f64.u32(x)/norm) xs) vct_ints

  module Reduce (X : { type t
                       val ne	: 	t
                       val op	: 	t -> t -> t
                       val f : [D]f64 -> t }) : { val run : i32 -> X.t } =
  {
    let run (N:i32) : X.t =
      reduce_stream_per X.op (\sz (ns:[sz]i32) : X.t ->
                           if sz > 0
                           then reduce X.op X.ne (map X.f (unsafe chunk ns[0] sz))
                           else X.ne)
      (iota N)

  }
}

module S8 = Sobol x.sobol_dir { let D = 8 }
module S2 = Sobol x.sobol_dir { let D = 2 }

let mean [n] (xs: [n]f64) : f64 =
  reduce (+) 0.0 xs / r64(n)

module R = S2.Reduce { type t = f64
                       let ne = 0f64
 		       let op (x:f64) (y:f64) = x f64.+ y
		       let f (v : [S2.D]f64) : f64 =
                         let x = v[0]
		 	 let y = v[1]
			 in f64.bool(x*x+y*y < 1f64) }

let pi (n:i32) : f64 =
  R.run n * 4.0 / r64(n)

let main (n: i32) : f64 = pi 10000
