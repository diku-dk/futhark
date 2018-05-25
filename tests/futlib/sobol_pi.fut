-- ==
-- entry: main
-- input { 1000 } output { 3.142000f64 }

import "/futlib/sobol"
import "/futlib/array"
import "/futlib/sobol-dir-50"

module S2 = Sobol sobol_dir { let D = 2 }

let pi (n:i32) : f64 =
  let v = S2.redomap (\ (v:[2]f64) ->
                      let x = v[0]
	              let y = v[1]
	              in if x*x+y*y < 1f64 then 1.0 else 0.0 ) (f64.+) 0f64 n
  in v * 4.0 / r64(n)

let main (n:i32) : f64 = pi n

-- let main () : [][2]f64 =
--  let as = [1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000]
--  in map (\x -> [r64 x,pi x]) as
