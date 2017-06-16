import "/futlib/math"
import "/futlib/sobol"
import "/futlib/sobol-dir-50"
module array = import "/futlib/array"

module S3 = Sobol sobol_dir { let D = 3 }
module S2 = Sobol sobol_dir { let D = 2 }

module R = S2.Reduce { type t = f64
                       let ne = 0f64
                       let op (x:f64) (y:f64) = x f64.+ y
                       let f (v : [2]f64) : f64 =
                         let x = v[0]
                         let y = v[1]
                         in f64(x*x+y*y < 1f64) }

-- ==
-- entry: test_chunk
-- input { 5 } output { [[0.0,0.5,0.75,0.25,0.375],[0.0,0.5,0.25,0.75,0.375],[0.0,0.5,0.25,0.75,0.625]] }

entry test_chunk (n:i32) : [][]f64 =
  array.transpose (S3.chunk 0 n)

-- ==
-- entry: test_pi
-- input { 1000 } output { true }

entry test_pi (n:i32) : bool =
  f64.abs(R.run n * 4.0 / f64(n) - 3.14) < 0.01
