-- Make sure not to share the in-place updated array too much in the
-- prelude.
-- ==
-- tags { no_webgpu }
-- random input { [10]f32 [100]f32 } auto output

def seqmap [n] 'a 'b (f: a -> b) (xs: [n]a) (dummy: b) : [n]b =
  loop out = replicate n dummy
  for i < n do
    out with [i] = f xs[i]

def main [n] (xs: [n]f32) is =
  #[sequential_inner]
  map (\i ->
         let arr = seqmap (+ i) (map f32.i64 (iota n)) 0
         in f32.sum (map2 (+) xs arr))
      is
