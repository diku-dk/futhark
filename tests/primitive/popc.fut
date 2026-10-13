-- ==
-- tags { no_webgpu }
-- entry: popci8
-- input { [0u64, 255u64, 65535u64, 4294967295u64, 18446744073709551615u64] }
-- output { [0i32, 8i32, 8i32, 8i32, 8i32] }

-- ==
-- entry: popci16
-- input { [0u64, 255u64, 65535u64, 4294967295u64, 18446744073709551615u64] }
-- output { [0i32, 8i32, 16i32, 16i32, 16i32] }

-- ==
-- entry: popci32
-- input { [0u64, 255u64, 65535u64, 4294967295u64, 18446744073709551615u64] }
-- output { [0i32, 8i32, 16i32, 32i32, 32i32] }

-- ==
-- entry: popci64
-- input { [0u64, 255u64, 65535u64, 4294967295u64, 18446744073709551615u64] }
-- output { [0i32, 8i32, 16i32, 32i32, 64i32] }

-- ==
-- entry: popcu8
-- input { [0u64, 255u64, 65535u64, 4294967295u64, 18446744073709551615u64] }
-- output { [0i32, 8i32, 8i32, 8i32, 8i32] }

-- ==
-- entry: popcu16
-- input { [0u64, 255u64, 65535u64, 4294967295u64, 18446744073709551615u64] }
-- output { [0i32, 8i32, 16i32, 16i32, 16i32] }

-- ==
-- entry: popcu32
-- input { [0u64, 255u64, 65535u64, 4294967295u64, 18446744073709551615u64] }
-- output { [0i32, 8i32, 16i32, 32i32, 32i32] }

-- ==
-- entry: popcu64
-- input { [0u64, 255u64, 65535u64, 4294967295u64, 18446744073709551615u64] }
-- output { [0i32, 8i32, 16i32, 32i32, 64i32] }

entry popci8 = map (\x -> i8.popc (i8.u64 x))
entry popci16 = map (\x -> i16.popc (i16.u64 x))
entry popci32 = map (\x -> i32.popc (i32.u64 x))
entry popci64 = map (\x -> i64.popc (i64.u64 x))
entry popcu8 = map (\x -> u8.popc (u8.u64 x))
entry popcu16 = map (\x -> u16.popc (u16.u64 x))
entry popcu32 = map (\x -> u32.popc (u32.u64 x))
entry popcu64 = map (\x -> u64.popc (u64.u64 x))
