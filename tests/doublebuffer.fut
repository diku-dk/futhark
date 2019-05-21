-- A simple program that needs double buffering to be compiled to
-- OpenCL code.

let main (n: i32) (xss: [][]i32) =
  map (iterate_while (\xs -> unsafe xs[0] < 10) (map (+1)))
