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
        num.(reduce (+) (i32 0) xs / i32 n))
}

-- ==
-- entry: test_i32_rand
-- compiled input { 0 10000 } output { 28 50 }
-- compiled input { 1 10000 } output { 37 50 }

module test_i32_rand_m =
 mktest minstd_rand i32 (uniform_int_distribution i32 minstd_rand)

entry test_i32_rand (x: i32) (n: i32) = test_i32_rand_m.test x n (1,100)

-- ==
-- entry: test_f32_rand
-- compiled input { 0 10000 } output { 28.439787f32 50.383465f32 }
-- compiled input { 1 10000 } output { 36.858000f32 50.435020f32 }

module test_f32_rand_m =
 mktest minstd_rand f32 (uniform_real_distribution f32 minstd_rand)

entry test_f32_rand (x: i32) (n: i32) =
  test_f32_rand_m.test x n (1f32,100f32)

-- ==
-- entry: test_f32_normal
-- compiled input { 0 10000 } output { 236.16484f32 51.118332f32 }
-- compiled input { 1 10000 } output { 144.83818f32 51.320827f32 }

module test_f32_normal_m =
 mktest xorshift128plus f32 (normal_distribution f32 xorshift128plus)

entry test_f32_normal (x: i32) (n: i32) =
  test_f32_normal_m.test x n {mean=50f32,stddev=25f32}

-- ==
-- entry: test_f32_shuffle
-- compiled input { 0 10000 } output { 165.477264f32 49.911663f32 }
-- compiled input { 1 10000 } output { 81.004059f32  52.509270f32 }

module shuffle_m =
 shuffle_order_engine {let k = 10} u64 xorshift128plus

module test_f32_shuffle_m =
  mktest shuffle_m f32 (normal_distribution f32 shuffle_m)

entry test_f32_shuffle (x: i32) (n: i32) =
  test_f32_shuffle_m.test x n {mean=50f32,stddev=25f32}
