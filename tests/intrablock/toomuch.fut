-- Intra-group for this one probably exceeds local memory
-- availability, so should not be picked, even if it looks like a good
-- idea!
-- ==
-- compiled random input { [128][256]f64 [128][256]f64 [128][256]f64 [128][256]f64 } auto output

type vec = (f64, f64, f64, f64)

def vecadd (a: vec) (b: vec) =
  (a.0 + b.0, a.1 + b.1, a.2 + b.2, a.3 + b.3)

def psum = scan vecadd (0, 0, 0, 0)

def main (xss: [][]f64) (yss: [][]f64) (zss: [][]f64) (vss: [][]f64) =
  #[incremental_flattening(no_outer)]
  map (psum >-> psum >-> psum >-> psum >-> psum >-> psum >-> psum >-> psum >-> psum)
      (map4 zip4 xss yss zss vss)
  |> map unzip4
  |> unzip4
