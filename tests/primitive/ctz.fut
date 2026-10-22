-- ==
-- tags { no_webgpu }
-- entry: ctzi8
-- input { [0i8, 255i8, 128i8] } output { [8, 0, 7] }

-- ==
-- entry: ctzi16
-- input { [0i16, 65535i16, 32768i16] } output { [16, 0, 15] }

-- ==
-- entry: ctzi32
-- input { [0i32, 4294967295i32, 2147483648i32] } output { [32, 0, 31] }

-- ==
-- entry: ctzi64
-- input { [0i64, 18446744073709551615i64, 9223372036854775808i64] } output { [64, 0, 63] }

entry ctzi8 = map i8.ctz
entry ctzi16 = map i16.ctz
entry ctzi32 = map i32.ctz
entry ctzi64 = map i64.ctz
