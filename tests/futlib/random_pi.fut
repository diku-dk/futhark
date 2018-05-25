-- ==
-- entry: main
-- input { 1000 } output { 3.124800f64 }

import "/futlib/random"
import "/futlib/array"

module rng_reduce (rng : rng_engine with int.t = u32) = {

  let redomap 't (D:i32) (f:[]f64->t) (g:t->t->t) (ne:t) (N:i32) : t =
    let seed = [61i32]
    let rng = rng.rng_from_seed seed
    let rngs = rng.split_rng N rng
    let m : [N][D]f64 =
      map1 (\ rng -> let rngs' = rng.split_rng D rng
                     in map1 (\rng -> let (_,v) = rng.rand rng
                                      in (r64 (i32.u32 v) - r64 (i32.u32 rng.min)) /
                                         (r64 (i32.u32 rng.max) - r64 (i32.u32 rng.min))
                             ) rngs') rngs
    let ts : [N]t = map f m
    in reduce g ne ts
}

module R = rng_reduce minstd_rand

let pi (n:i32) : f64 =
    let v = R.redomap 2 (\ (v:[2]f64) ->
                         let x = v[0]
	                 let y = v[1]
	                 in if x*x+y*y < 1f64 then 1.0 else 0.0 )
                        (f64.+) 0f64 n
    in v * 4.0 / r64(n)

let main (n:i32) : f64 = pi n

-- let main () : [][2]f64 =
--  let as = [1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000]
--  in map (\x -> [r64 x,pi x]) as
