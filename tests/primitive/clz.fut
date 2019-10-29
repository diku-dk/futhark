-- ==
-- input { 0u64 } output { 8i32 16i32 32i32 64i32 8i32 16i32 32i32 64i32 }
-- input { 255u64 } output { 0i32 8i32 24i32 56i32 0i32 8i32 24i32 56i32 }
-- input { 65535u64 } output { 0i32 0i32 16i32 48i32 0i32 0i32 16i32 48i32 }
-- input { 4294967295u64 } output { 0i32 0i32 0i32 32i32 0i32 0i32 0i32 32i32 }
-- input { 18446744073709551615u64 } output { 0i32 0i32 0i32 0i32 0i32 0i32 0i32 0i32 }

let main (x: u64) =
  (i8.clz (i8.u64 x),
   i16.clz (i16.u64 x),
   i32.clz (i32.u64 x),
   i64.clz (i64.u64 x),
   u8.clz (u8.u64 x),
   u16.clz (u16.u64 x),
   u32.clz (u32.u64 x),
   u64.clz (u64.u64 x))
