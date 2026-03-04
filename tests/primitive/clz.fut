-- ==
-- tags { no_webgpu }
-- entry: clzi8
-- input { [0u64, 255u64, 65535u64, 4294967295u64, 18446744073709551615u64] } output { [8i32, 0i32, 0i32, 0i32, 0i32] }

-- ==
-- entry: clzi16
-- input { [0u64, 255u64, 65535u64, 4294967295u64, 18446744073709551615u64] } output { [16i32, 8i32, 0i32, 0i32, 0i32] }

-- ==
-- entry: clzi32
-- input { [0u64, 255u64, 65535u64, 4294967295u64, 18446744073709551615u64] } output { [32i32, 24i32, 16i32, 0i32, 0i32] }

-- ==
-- entry: clzi64
-- input { [0u64, 255u64, 65535u64, 4294967295u64, 18446744073709551615u64] } output { [64i32, 56i32, 48i32, 32i32, 0i32] }

-- ==
-- entry: clzu8
-- input { [0u64, 255u64, 65535u64, 4294967295u64, 18446744073709551615u64] } output { [8i32, 0i32, 0i32, 0i32, 0i32] }

-- ==
-- entry: clzu16
-- input { [0u64, 255u64, 65535u64, 4294967295u64, 18446744073709551615u64] } output { [16i32, 8i32, 0i32, 0i32, 0i32] }

-- ==
-- entry: clzu32
-- input { [0u64, 255u64, 65535u64, 4294967295u64, 18446744073709551615u64] } output { [32i32, 24i32, 16i32, 0i32, 0i32] }

-- ==
-- entry: clzu64
-- input { [0u64, 255u64, 65535u64, 4294967295u64, 18446744073709551615u64] } output { [64i32, 56i32, 48i32, 32i32, 0i32] }

entry clzi8 = map (\x -> i8.clz (i8.u64 x))
entry clzi16 = map (\x -> i16.clz (i16.u64 x))
entry clzi32 = map (\x -> i32.clz (i32.u64 x))
entry clzi64 = map (\x -> i64.clz (i64.u64 x))
entry clzu8 = map (\x -> u8.clz (u8.u64 x))
entry clzu16 = map (\x -> u16.clz (u16.u64 x))
entry clzu32 = map (\x -> u32.clz (u32.u64 x))
entry clzu64 = map (\x -> u64.clz (u64.u64 x))
