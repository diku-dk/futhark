-- A simple program that needs double buffering to be compiled to
-- OpenCL code.

def main (n: i32) (xss: [][]i32) =
  map (\xs -> #[unsafe] #[sequential] iterate_while (\xs -> xs[0] < 10) (map (+ 1)) xs)
