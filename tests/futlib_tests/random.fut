import "/futlib/math"
import "/futlib/random"

module mktest (engine: rng_engine) (num: numeric)
              (dist: rng_distribution with rng = engine.rng with t = num.t) = {

  let test (x: i32) (n: i32) (d: dist.distribution) =
    let rng = engine.rng_from_seed [x]
    let (rng, x) = dist.rand d rng
    let rngs = engine.split_rng n rng
    let (_, xs) = unzip (map (dist.rand d) rngs)
    in (x,
        num.(reduce (+) (from_i64 0i64) xs / from_i64 (i64 n)))
}

-- ==
-- entry: test_i32_rand
-- input { 0 10000 } output { 28 50 }
-- input { 1 10000 } output { 37 50 }

module test_i32_rand =
 mktest minstd_rand i32 (uniform_int_distribution i32 minstd_rand)

entry test_i32_rand (x: i32) (n: i32) = test_i32_rand.test x n (1,100)

-- ==
-- entry: test_f32_rand
-- input { 0 10000 } output { 28.439787f32 50.383465f32 }
-- input { 1 10000 } output { 36.858000f32 50.435020f32 }

module test_f32_rand =
 mktest minstd_rand f32 (uniform_real_distribution f32 minstd_rand)

entry test_f32_rand (x: i32) (n: i32) =
  test_f32_rand.test x n (1f32,100f32)

-- ==
-- entry: test_f32_normal
-- input { 0 10000 } output { 236.16484f32 51.118332f32 }
-- input { 1 10000 } output { 144.83818f32 51.320827f32 }

module test_f32_normal =
 mktest xorshift128plus f32 (normal_distribution f32 xorshift128plus)

entry test_f32_normal (x: i32) (n: i32) =
  test_f32_normal.test x n {mean=50f32,stddev=25f32}
