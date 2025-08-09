-- Test the set_bit and get_bit functions for unsigned integers.
-- ==
-- entry: test_u8_get
-- input { [8u8, 8u8, 24u8, 0b010101u8, 0b11111111u8]
--         [3,   2,   3,    3,          7] }
-- output { [1,  0,   1,    0,          1] }

-- ==
-- entry: test_u8_set0
-- input { [8u8, 8u8, 24u8, 0b010101u8, 0b11111111u8]
--         [3,   2,   3,    3,          7] }
-- output { [0u8, 8u8, 16u8, 0b010101u8, 0b01111111u8] }

-- ==
-- entry: test_u8_set1
-- input { [8u8, 8u8, 24u8, 0b010101u8, 0b11111111u8]
--         [3,   2,   3,    3,          7] }
-- output { [8u8, 12u8, 24u8, 0b011101u8, 0b11111111u8] }

entry test_u8_get = map2 (\a bit -> u8.get_bit bit a)
entry test_u8_set0 = map2 (\a bit -> u8.set_bit bit a 0)
entry test_u8_set1 = map2 (\a bit -> u8.set_bit bit a 1)

-- ==
-- entry: test_u16_get
-- input { [8u16, 8u16, 24u16, 0b0011001001010101u16, 0b1011011010010010u16]
--         [3,    2,    3,     11,                    13] }
-- output { [1,   0,    1,     0,                     1] }

-- ==
-- entry: test_u16_set0
-- input { [8u16, 8u16, 24u16, 0b0011001001010101u16, 0b1011011010010010u16]
--         [3,    2,    3,     11,                    13] }
-- output { [0u16, 8u16, 16u16, 0b0011001001010101u16, 0b1001011010010010u16] }

-- ==
-- entry: test_u16_set1
-- input { [8u16, 8u16, 24u16, 0b0011001001010101u16, 0b1011011010010010u16]
--         [3,    2,    3,     11,                    13] }
-- output { [8u16, 12u16, 24u16, 0b0011101001010101u16, 0b1011011010010010u16] }

entry test_u16_get = map2 (\a bit -> u16.get_bit bit a)
entry test_u16_set0 = map2 (\a bit -> u16.set_bit bit a 0)
entry test_u16_set1 = map2 (\a bit -> u16.set_bit bit a 1)

-- ==
-- entry: test_u32_get
-- input { [8u32, 8u32, 24u32, 0b0011001001010101u32, 0b11111111u32]
--         [3,    2,    3,     11,                    7] }
-- output { [1,   0,    1,     0,                     1] }

-- ==
-- entry: test_u32_set0
-- input { [8u32, 8u32, 24u32, 0b0011001001010101u32, 0b11111111u32]
--         [3,    2,    3,     11,                    7] }
-- output { [0u32, 8u32, 16u32, 0b0011001001010101u32, 0b01111111u32] }

-- ==
-- entry: test_u32_set1
-- input { [8u32, 8u32, 24u32, 0b0011001001010101u32, 0b11111111u32]
--         [3,    2,    3,     11,                    7] }
-- output { [8u32, 12u32, 24u32, 0b0011101001010101u32, 0b11111111u32] }

entry test_u32_get = map2 (\a bit -> u32.get_bit bit a)
entry test_u32_set0 = map2 (\a bit -> u32.set_bit bit a 0)
entry test_u32_set1 = map2 (\a bit -> u32.set_bit bit a 1)

-- ==
-- entry: test_u64_get
-- input { [8u64, 8u64, 24u64, 0b0011001001010101u64, 0b11111111u64, 4294967295u64, 4294967295u64]
--         [3,    2,    3,     11,                    7,             31,            30] }
-- output { [1,   0,    1,     0,                     1,             1,             1] }

-- ==
-- entry: test_u64_set0
-- input { [8u64, 8u64, 24u64, 0b0011001001010101u64, 0b11111111u64, 4294967295u64, 4294967295u64]
--         [3,    2,    3,     11,                    7,             31,            30] }
-- output { [0u64, 8u64, 16u64, 0b0011001001010101u64, 0b01111111u64, 2147483647u64, 3221225471u64] }

-- ==
-- entry: test_u64_set1
-- input { [8u64, 8u64, 24u64, 0b0011001001010101u64, 0b11111111u64, 4294967295u64, 4294967295u64]
--         [3,    2,    3,     11,                    7,             31,            30] }
-- output { [8u64, 12u64, 24u64, 0b0011101001010101u64, 0b11111111u64, 4294967295u64, 4294967295u64] }

entry test_u64_get = map2 (\a bit -> u64.get_bit bit a)
entry test_u64_set0 = map2 (\a bit -> u64.set_bit bit a 0)
entry test_u64_set1 = map2 (\a bit -> u64.set_bit bit a 1)
