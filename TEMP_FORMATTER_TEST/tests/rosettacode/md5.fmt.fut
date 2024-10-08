-- MD5 implementation.  Based on Accelerate and other sources.
--
-- Easy to get wrong if you forget that MD5 is little-endian.
--
-- Ignored on ISPC backend since the ISPC compiler miscompiles the program
-- when using 64-bit addressing mode. Memory reads from valid addresses
-- cause a segfault if there are inactive lanes with invalid addresses.
-- ==
-- no_ispc input { empty([0]u8) }
-- output { [0xd4u8,0x1du8,0x8cu8,0xd9u8,0x8fu8,0x00u8,0xb2u8,0x04u8,0xe9u8,0x80u8,0x09u8,0x98u8,0xecu8,0xf8u8,0x42u8,0x7eu8] }
-- no_ispc input { [0u8] }
-- output { [0x93u8, 0xb8u8, 0x85u8, 0xadu8, 0xfeu8, 0x0du8, 0xa0u8, 0x89u8, 0xcdu8, 0xf6u8, 0x34u8, 0x90u8, 0x4fu8, 0xd5u8, 0x9fu8, 0x71u8] }
type md5 = (u32, u32, u32, u32)

def us32 (x: i32) = u32.i32 x

def rs: [64]u32 =
  map
    us32 [7
    ,12
    ,17
    ,22
    ,7
    ,12
    ,17
    ,22
    ,7
    ,12
    ,17
    ,22
    ,7
    ,12
    ,17
    ,22
    ,5
    ,9
    ,14
    ,20
    ,5
    ,9
    ,14
    ,20
    ,5
    ,9
    ,14
    ,20
    ,5
    ,9
    ,14
    ,20
    ,4
    ,11
    ,16
    ,23
    ,4
    ,11
    ,16
    ,23
    ,4
    ,11
    ,16
    ,23
    ,4
    ,11
    ,16
    ,23
    ,6
    ,10
    ,15
    ,21
    ,6
    ,10
    ,15
    ,21
    ,6
    ,10
    ,15
    ,21
    ,6
    ,10
    ,15
    ,21]

def ks: [64]u32 =
  [3614090360
  ,3905402710
  ,606105819
  ,3250441966
  ,4118548399
  ,1200080426
  ,2821735955
  ,4249261313
  ,1770035416
  ,2336552879
  ,4294925233
  ,2304563134
  ,1804603682
  ,4254626195
  ,2792965006
  ,1236535329
  ,4129170786
  ,3225465664
  ,643717713
  ,3921069994
  ,3593408605
  ,38016083
  ,3634488961
  ,3889429448
  ,568446438
  ,3275163606
  ,4107603335
  ,1163531501
  ,2850285829
  ,4243563512
  ,1735328473
  ,2368359562
  ,4294588738
  ,2272392833
  ,1839030562
  ,4259657740
  ,2763975236
  ,1272893353
  ,4139469664
  ,3200236656
  ,681279174
  ,3936430074
  ,3572445317
  ,76029189
  ,3654602809
  ,3873151461
  ,530742520
  ,3299628645
  ,4096336452
  ,1126891415
  ,2878612391
  ,4237533241
  ,1700485571
  ,2399980690
  ,4293915773
  ,2240044497
  ,1873313359
  ,4264355552
  ,2734768916
  ,1309151649
  ,4149444226
  ,3174756917
  ,718787259
  ,3951481745]

def rotate_left (x: u32, c: u32): u32 = (x << c) | (x >> (32u32 - c))

def bytes (x: u32): [4]u8 =
  [u8.u32 (x)
  ,u8.u32 (x / 256u32)
  ,u8.u32 (x / 65536u32)
  ,u8.u32 (x / 16777216u32)]

def unbytes (bs: [4]u8): u32 =
  u32.u8 (bs[0])
  + u32.u8 (bs[1]) * 256u32
  + u32.u8 (bs[2]) * 65536u32
  + u32.u8 (bs[3]) * 16777216u32

def unbytes_block (block: [16 * 4]u8): [16]u32 =
  map unbytes (unflatten block)

-- Process 512 bits of the input.
def md5_chunk ((a0, b0, c0, d0): md5) (m: [16]u32): md5 =
  loop (a, b, c, d) = (a0, b0, c0, d0) for i < 64 do
    let (f, g) =
      if i < 16 then
      ((b & c) | (notb & d)
        ,u32.i32 i)
      else
      if i < 32 then
        ((d & b) | (notd & c)
          ,(5u32 * u32.i32 i + 1u32) % 16u32)
        else
        if i < 48 then
          (b ^ c ^ d
            ,(3u32 * u32.i32 i + 5u32) % 16u32)
          else
          (c ^ (b | notd)
            ,(7u32 * u32.i32 i) % 16u32)
    in (d, b + rotate_left (a + f + ks[i] + m[i32.u32 g], rs[i]), b, c)

def md5 [n] (ms: [n][16]u32): md5 =
  let a0 = 1732584193u32
  let b0 = 4023233417u32
  let c0 = 2562383102u32
  let d0 = 271733878u32
  in
    loop ((a0, b0, c0, d0)) = (a0, b0, c0, d0) for i < n do
      let (a, b, c, d) = md5_chunk (a0, b0, c0, d0) ms[i]
      in (a0 + a, b0 + b, c0 + c, d0 + d)

def main [n] (ms: [n]u8): [16]u8 =
  let padding = 64 - (n % 64)
  let n_padded = n + padding
  let num_blocks = n_padded / 64
  let ms_padded =
    ms
    ++ bytes 128u32
    ++ replicate (padding - 12) 0u8
    ++ bytes (u32.i64 (n * 8))
    ++ [0u8, 0u8, 0u8, 0u8] :> [num_blocks * (16 * 4)]u8
  let (a, b, c, d) = md5 (map unbytes_block (unflatten ms_padded))
  in flatten (map bytes [a, b, c, d]) :> [16]u8