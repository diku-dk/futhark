-- Iota with some different types.
-- ==
-- input { 2 }
-- output { [0u8,1u8] [0u16,1u16] [0u32,1u32] [0u64,1u64]
--          [0u8,1u8] [0u16,1u16] [0u32,1u32] [0u64,1u64]
--        }

let main(n: i32): ([]i8, []i16, []i32, []i64, []u8, []u16, []u32, []u64) =
  (iota (i8 n), iota (i16 n), iota (i32 n), iota (i64 n),
   iota (u8 n), iota (u16 n), iota (u32 n), iota (u64 n))
