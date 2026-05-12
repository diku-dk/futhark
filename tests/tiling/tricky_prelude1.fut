-- Make sure not to share the in-place updated array too much in the
-- prelude.
-- ==
-- random input { [10]f64 [100]f64 } auto output

def seqmap [n] 'a 'b (f: a -> b) (xs: [n]a) (dummy: b) : [n]b =
  loop out = replicate n dummy
  for i < n do
    out with [i] = f xs[i]

def main [n] (xs: [n]f64) is =
  #[sequential_inner]
  map (\i ->
         let arr = seqmap (+ i) (map f64.i64 (iota n)) 0
         in f64.sum (map2 (+) xs arr))
      is
