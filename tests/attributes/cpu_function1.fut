-- Like cpu_function0.fut, but now the function is used in a parallel context.
-- In this case we currently ignore the attribute.
-- ==
-- tags { no_ispc }
-- input { [[1,2,3]] }
-- output { [[3,4,5]] }
-- structure gpu-mem { Manifest 0 SegMap 1 Loop 0 }

#[noinline] #[cpu_function]
def frob [n] (xs: [n]i32) =
  map (+ 2) xs

entry main (xss: [][]i32) = map frob xss
