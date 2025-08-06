-- ==
-- input { [0x2b28ab09u32, 0x7eaef7cfu32, 0x15d2154fu32, 0x16a6883cu32] }
-- auto output

def blk_transpose (block: [4]u32) : [4]u32 =
  #[sequential]
  map (\i ->
         let offset = u32.i64 (3 - i) << 3
         in (((block[0] >> offset) & 0xFF) << 24)
            | (((block[1] >> offset) & 0xFF) << 16)
            | (((block[2] >> offset) & 0xFF) << 8)
            | ((block[3] >> offset) & 0xFF))
      (iota 4)

def main (key: [4]u32) : [11][4]u32 =
  map blk_transpose (loop w = [key] for _i < 10i32 do (w ++ [key])) :> [11][4]u32
