-- Distilled from GenericPricing.
--
-- This test does not actually have anything to do with split, but
-- rather exposed a bug in array index generation due to a typo in the
-- scalar expression module (see commit
-- d4a3f6f313deb2d246c15c30bcb095afa1095338).  This test still has
-- value since apparently no other part of the test suite triggered
-- this code path.
-- ==
-- input {
--   [ [[ 1.0000000 , 0.6000000 , 0.8000000  ],
--      [ 0.6000000 , 0.8000000 , 0.1500000  ],
--      [ 0.8000000 , 0.1500000 , 0.5809475  ]]
--   ]
-- }
-- output {
--   [0.000000, 0.600000, 0.950000]
-- }

fun take(n: int, a: []f64): []f64 =
  let (first, rest) = unsafe split( (n), a) in
  first

fun fftmp(md_c: [n][]f64): []f64 =
  map( fn (j: int): f64  =>
         let x = take(j,md_c[j])
         in  reduce(+, 0.0, x),
       iota(n)
     )

fun main(all_md_c: [][][]f64): []f64 =
  let md_c = all_md_c[0] in
  fftmp(md_c)
