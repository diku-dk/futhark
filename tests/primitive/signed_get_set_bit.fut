-- Test the set_bit and get_bit functions for signed integers.
-- ==
-- entry: test_i8_get
-- input { [8i8, 8i8, 24i8, 0b010101i8, 0b11111111i8]
--         [3,   2,   3,    3,          7] }
-- output { [1,  0,   1,    0,          1] }

-- ==
-- entry: test_i8_set0
-- input { [8i8, 8i8, 24i8, 0b010101i8, 0b11111111i8]
--         [3,   2,   3,    3,          7] }
-- output { [0i8, 8i8, 16i8, 0b010101i8, 0b01111111i8] }

-- ==
-- entry: test_i8_set1
-- input { [8i8, 8i8, 24i8, 0b010101i8, 0b11111111i8]
--         [3,   2,   3,    3,          7] }
-- output { [8i8, 12i8, 24i8, 0b011101i8, 0b11111111i8] }

entry test_i8_get = map2 (\a bit -> i8.get_bit bit a)
entry test_i8_set0 = map2 (\a bit -> i8.set_bit bit a 0)
entry test_i8_set1 = map2 (\a bit -> i8.set_bit bit a 1)

-- ==
-- entry: test_i16_get
-- input { [8i16, 8i16, 24i16, 0b0011001001010101i16, 0b1011011010010010i16]
--         [3,    2,    3,     11,                    13] }
-- output { [1,   0,    1,     0,                     1] }

-- ==
-- entry: test_i16_set0
-- input { [8i16, 8i16, 24i16, 0b0011001001010101i16, 0b1011011010010010i16]
--         [3,    2,    3,     11,                    13] }
-- output { [0i16, 8i16, 16i16, 0b0011001001010101i16, 0b1001011010010010i16] }

-- ==
-- entry: test_i16_set1
-- input { [8i16, 8i16, 24i16, 0b0011001001010101i16, 0b1011011010010010i16]
--         [3,    2,    3,     11,                    13] }
-- output { [8i16, 12i16, 24i16, 0b0011101001010101i16, 0b1011011010010010i16] }

entry test_i16_get = map2 (\a bit -> i16.get_bit bit a)
entry test_i16_set0 = map2 (\a bit -> i16.set_bit bit a 0)
entry test_i16_set1 = map2 (\a bit -> i16.set_bit bit a 1)

-- ==
-- entry: test_i32_get
-- input { [8i32, 8i32, 24i32, 214783648i32, 214783648i32]
--         [3,    2,    3,     5,            11] }
-- output { [1,   0,    1,     1,            0] }

-- ==
-- entry: test_i32_set0
-- input { [8i32, 8i32, 24i32, 214783648i32, 214783648i32]
--         [3,    2,    3,     5,            11] }
-- output { [0i32, 8i32, 16i32, 214783616i32, 214783648i32] }

-- ==
-- entry: test_i32_set1
-- input { [8i32, 8i32, 24i32, 214783648i32, 214783648i32]
--         [3,    2,    3,     5,            11] }
-- output { [8i32, 12i32, 24i32, 214783648i32, 214785696i32] }

entry test_i32_get = map2 (\a bit -> i32.get_bit bit a)
entry test_i32_set0 = map2 (\a bit -> i32.set_bit bit a 0)
entry test_i32_set1 = map2 (\a bit -> i32.set_bit bit a 1)

-- ==
-- entry: test_i64_get
-- input { [8i64, 8i64, 24i64, 4294967295i64, 4294967295i64]
--         [3,    2,    3,     31,            30] }
-- output { [1,   0,    1,     1,             1] }

-- ==
-- entry: test_i64_set0
-- input { [8i64, 8i64, 24i64, 4294967295i64, 4294967295i64]
--         [3,    2,    3,     31,            30] }
-- output { [0i64, 8i64, 16i64, 2147483647i64, 3221225471i64] }

-- ==
-- entry: test_i64_set1
-- input { [8i64, 8i64, 24i64, 4294967295i64, 4294967295i64]
--         [3,    2,    3,     31,            30] }
-- output { [8i64, 12i64, 24i64, 4294967295i64, 4294967295i64] }

entry test_i64_get = map2 (\a bit -> i64.get_bit bit a)
entry test_i64_set0 = map2 (\a bit -> i64.set_bit bit a 0)
entry test_i64_set1 = map2 (\a bit -> i64.set_bit bit a 1)
