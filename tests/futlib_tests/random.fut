import "/futlib/math"
import "/futlib/random"

module mktest (dist: rng_distribution) = {

  module engine = dist.engine
  module num = dist.num

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
 mktest (uniform_int_distribution i32 minstd_rand)

entry test_i32_rand (x: i32) (n: i32) = test_i32_rand_m.test x n (1,100)

-- ==
-- entry: test_i32_ranlux24_base
-- compiled input { 0 10000 } output { 29 50 }
-- compiled input { 1 10000 } output { 96 50 }

module test_i32_ranlux24_base_m =
  mktest (uniform_int_distribution i32 ranlux24_base)

entry test_i32_ranlux24_base (x: i32) (n: i32) =
  test_i32_ranlux24_base_m.test x n (1,100)

-- ==
-- entry: test_i32_ranlux24
-- compiled input { 0 10000 } output { 29 50 }
-- compiled input { 1 10000 } output { 96 50 }

module test_i32_ranlux24_m =
  mktest (uniform_int_distribution i32 ranlux24)

entry test_i32_ranlux24 (x: i32) (n: i32) =
  test_i32_ranlux24_m.test x n (1,100)

-- ==
-- entry: test_i32_pcg32
-- compiled input { 0 10000 } output { 5 50 }
-- compiled input { 1 10000 } output { 94 50 }

module test_i32_pcg32_m =
  mktest (uniform_int_distribution i32 pcg32)

entry test_i32_pcg32 (x: i32) (n: i32) =
  test_i32_pcg32_m.test x n (1,100)

-- ==
-- entry: test_f32_rand
-- compiled input { 0 10000 } output { 28.439787f32 50.383465f32 }
-- compiled input { 1 10000 } output { 36.858000f32 50.435020f32 }

module test_f32_rand_m =
 mktest (uniform_real_distribution f32 minstd_rand)

entry test_f32_rand (x: i32) (n: i32) =
  test_f32_rand_m.test x n (1f32,100f32)

-- ==
-- entry: test_f32_normal
-- compiled input { 0 10000 } output { 87.695510f32 50.054848f32 }
-- compiled input { 1 10000 } output { 69.860374f32 49.907085f32 }

module test_f32_normal_m =
 mktest (normal_distribution f32 xorshift128plus)

entry test_f32_normal (x: i32) (n: i32) =
  test_f32_normal_m.test x n {mean=50f32,stddev=25f32}

-- ==
-- entry: test_f32_shuffle
-- compiled input { 0 10000 } output { 74.1559750f32 50.02034f32 }
-- compiled input { 1 10000 } output { 60.1060940f32 50.52343f32 }

module shuffle_m =
 shuffle_order_engine {let k = 10} xorshift128plus

module test_f32_shuffle_m =
  mktest (normal_distribution f32 shuffle_m)

entry test_f32_shuffle (x: i32) (n: i32) =
  test_f32_shuffle_m.test x n {mean=50f32,stddev=25f32}
