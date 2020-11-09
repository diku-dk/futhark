-- ==
-- input { 0u64 } output { 0i32 0i32 0i32 0i32 0i32 0i32 0i32 0i32 }
-- input { 255u64 } output { 8i32 8i32 8i32 8i32 8i32 8i32 8i32 8i32 }
-- input { 65535u64 } output { 8i32 16i32 16i32 16i32 8i32 16i32 16i32 16i32 }
-- input { 4294967295u64 } output { 8i32 16i32 32i32 32i32 8i32 16i32 32i32 32i32 }
-- input { 18446744073709551615u64 } output { 8i32 16i32 32i32 64i32 8i32 16i32 32i32 64i32 }

let main (x: u64) =
  (i8.popc (i8.u64 x),
   i16.popc (i16.u64 x),
   i32.popc (i32.u64 x),
   i64.popc (i64.u64 x),
   u8.popc (u8.u64 x),
   u16.popc (u16.u64 x),
   u32.popc (u32.u64 x),
   u64.popc (u64.u64 x))
