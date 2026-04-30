-- | ignore

import "random"

module mktest (dist: rng_distribution) = {
  module engine = dist.engine
  module num = dist.num

  def test (x: i32) (n: i64) (d: dist.distribution) =
    let rng = engine.rng_from_seed [x]
    let (rng, _) = dist.rand d rng
    let rngs = engine.split_rng n rng
    let (_, xs) = unzip (map (dist.rand d) rngs)
    let mean = num.(reduce (+) (i32 0) xs / i64 n)
    in mean
}

module mktest_f (dist: rng_distribution) (R: real with t = dist.num.t) = {
  module engine = dist.engine
  module num = dist.num

  def test (x: i32) (n: i64) (d: dist.distribution) =
    let rng = engine.rng_from_seed [x]
    let (rng, _) = dist.rand d rng
    let rngs = engine.split_rng n rng
    let (_, xs) = unzip (map (dist.rand d) rngs)
    let mean = num.(reduce (+) (i32 0) xs / i64 n)
    let stddev =
      R.(xs
         |> map (\x -> (x - mean))
         |> map num.((** i32 2))
         |> sum
         |> (/ i64 n)
         |> sqrt)
    in (R.round mean, R.round stddev)
}

-- ==
-- entry: test_i32_rand
-- compiled input { 0 10000i64 } output { 50 }
-- compiled input { 1 10000i64 } output { 50 }

module test_i32_rand_m =
  mktest (uniform_int_distribution i32 u32 minstd_rand)

entry test_i32_rand (x: i32) (n: i64) = test_i32_rand_m.test x n (1, 100)

-- Inspired by https://github.com/diku-dk/cpprandom/issues/2
-- ==
-- entry: test_i64_rand
-- compiled input { 0 10000i64 } output { -16 }
-- compiled input { 1 10000i64 } output { -15 }

module test_i64_rand_m =
  mktest (uniform_int_distribution i32 u32 minstd_rand)

entry test_i64_rand (x: i32) (n: i64) = test_i32_rand_m.test x n (-20, -10)

-- ==
-- entry: test_i32_ranlux24_base
-- compiled input { 0 10000i64 } output { 50 }
-- compiled input { 1 10000i64 } output { 50 }

module test_i32_ranlux24_base_m =
  mktest (uniform_int_distribution i32 u32 ranlux24_base)

entry test_i32_ranlux24_base (x: i32) (n: i64) =
  test_i32_ranlux24_base_m.test x n (1, 100)

-- ==
-- entry: test_i32_ranlux24
-- compiled input { 0 10000i64 } output { 50 }
-- compiled input { 1 10000i64 } output { 50 }

module test_i32_ranlux24_m =
  mktest (uniform_int_distribution i32 u32 ranlux24)

entry test_i32_ranlux24 (x: i32) (n: i64) =
  test_i32_ranlux24_m.test x n (1, 100)

-- ==
-- entry: test_i32_pcg32
-- compiled input { 0 10000i64 } output { 50 }
-- compiled input { 1 10000i64 } output { 50 }

module test_i32_pcg32_m =
  mktest (uniform_int_distribution i32 u32 pcg32)

entry test_i32_pcg32 (x: i32) (n: i64) =
  test_i32_pcg32_m.test x n (1, 100)

-- ==
-- entry: test_f32_rand
-- compiled input { 0 10000i64 } output { 50.719204f32 }
-- compiled input { 1 10000i64 } output { 50.019245f32 }

module test_f32_rand_m =
  mktest (uniform_real_distribution f32 u32 minstd_rand)

entry test_f32_rand (x: i32) (n: i64) =
  test_f32_rand_m.test x n (1f32, 100f32)

-- ==
-- entry: test_f32_normal
-- compiled input { 0 10000i64 } output { 50f32 25f32 }
-- compiled input { 1 10000i64 } output { 50f32 25f32 }

module test_f32_normal_m =
  mktest_f (normal_distribution f32 u64 xorshift128plus) f32

entry test_f32_normal (x: i32) (n: i64) =
  test_f32_normal_m.test x n {mean = 50f32, stddev = 25f32}

-- ==
-- entry: test_f32_shuffle
-- compiled input { 0 10000i64 } output { 50f32 25f32 }
-- compiled input { 1 10000i64 } output { 50f32 25f32 }

module shuffle_m =
  shuffle_order_engine {def k : i32 = 30} u32 pcg32

module test_f32_shuffle_m =
  mktest_f (normal_distribution f32 u32 shuffle_m) f32

entry test_f32_shuffle (x: i32) (n: i64) =
  test_f32_shuffle_m.test x n {mean = 50f32, stddev = 25f32}
