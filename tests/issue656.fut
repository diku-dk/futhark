-- This program fuses into (among other things) a streamSeq, and we
-- had a bug where the result of the first-order-transformed streamSeq
-- might alias some other array (specifically 'xs'), and the result
-- then went on to be consumed in the 'scatter's.

def main [n] (xs: [n]i32) (is: [n]i32) =
  let bits1 = map (& 1) xs
  let bits0 = map (1 -) bits1
  let idxs0 = map2 (*) bits0 (scan (+) 0 bits0)
  let idxs1 = scan (+) 0 bits1
  let offs = reduce (+) 0 bits0
  let idxs1 = map2 (*) bits1 (map (+ offs) idxs1)
  let idxs = map (\x -> x - 1) (map2 (+) idxs0 idxs1)
  in ( scatter (copy xs) (map i64.i32 idxs) xs
     , scatter (copy is) (map i64.i32 idxs) is
     )
