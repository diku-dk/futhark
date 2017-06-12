import "/futlib/math"
import "/futlib/random"

-- ==
-- entry: test_i32_rand
-- input { 0 10000 } output { 28 50 }
-- input { 1 10000 } output { 37 50 }

module i32_rand = uniform_int_distribution i32 minstd_rand

entry test_i32_rand (x: i32) (n: i32) =
  let rng = minstd_rand.rng_from_seed [x]
  let (rng, x) = i32_rand.rand (1,100) rng
  let rngs = minstd_rand.split_rng n rng
  let (_, xs) = unzip (map (i32_rand.rand (1,100)) rngs)
  in (x, reduce (+) 0 xs / n)

-- ==
-- entry: test_f32_rand
-- input { 0 10000 } output { 28.439787f32 50.383465f32 }
-- input { 1 10000 } output { 36.858000f32 50.435020f32 }

module f32_rand = uniform_real_distribution f32 minstd_rand

entry test_f32_rand (x: i32) (n: i32) =
  let rng = minstd_rand.rng_from_seed [x]
  let (rng, x) = f32_rand.rand (1f32,100f32) rng
  let rngs = minstd_rand.split_rng n rng
  let (_, xs) = unzip (map (f32_rand.rand (1f32,100f32)) rngs)
  in (x, reduce (+) 0f32 xs / f32 n)
