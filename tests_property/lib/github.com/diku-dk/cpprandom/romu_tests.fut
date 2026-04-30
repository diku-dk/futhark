-- | ignore

import "romu"

module util = import "random_tests"

module test_romu_quad_m = util.mktest (uniform_int_distribution i32 u64 romu_quad)

module test_romu_trio_m = util.mktest (uniform_int_distribution i32 u64 romu_trio)

module test_romu_duo_m = util.mktest (uniform_int_distribution i32 u64 romu_duo)

module test_romu_duo_jr_m = util.mktest (uniform_int_distribution i32 u64 romu_duo_jr)

module test_romu_quad32_m = util.mktest (uniform_int_distribution i32 u32 romu_quad32)

module test_romu_trio32_m = util.mktest (uniform_int_distribution i32 u32 romu_trio32)

module test_romu_mono32_m = util.mktest (uniform_int_distribution i32 u16 romu_mono32)

entry test_romu_quad (x: i32) (n: i64) = test_romu_quad_m.test x n (1, 100)
entry test_romu_trio (x: i32) (n: i64) = test_romu_trio_m.test x n (1, 100)
entry test_romu_duo (x: i32) (n: i64) = test_romu_duo_m.test x n (1, 100)
entry test_romu_duo_jr (x: i32) (n: i64) = test_romu_duo_jr_m.test x n (1, 100)

entry test_romu_quad32 (x: i32) (n: i64) = test_romu_quad32_m.test x n (1, 100)
entry test_romu_trio32 (x: i32) (n: i64) = test_romu_trio32_m.test x n (1, 100)
entry test_romu_mono32 (x: i32) (n: i64) = test_romu_mono32_m.test x n (1, 100)

-- ==
-- entry: test_romu_quad test_romu_trio test_romu_duo test_romu_duo_jr test_romu_quad32 test_romu_trio32 test_romu_mono32
-- compiled input { 0 1000000i64 } output { 50 }
-- compiled input { 1 1000000i64 } output { 50 }
