-- MD5 implementation.  Based on Accelerate and other sources.
--
-- Easy to get wrong if you forget that MD5 is little-endian.
--
-- Ignored on ISPC backend since the ISPC compiler miscompiles the program
-- when using 64-bit addressing mode. Memory reads from valid addresses
-- cause a segfault if there are inactive lanes with invalid addresses.
-- ==
-- no_ispc input { empty([0]u8) }
-- output { [0xd4u8,0x1d,0x8c,0xd9,0x8f,0x00,0xb2,0x04,0xe9,0x80,0x09,0x98,0xec,0xf8,0x42,0x7e] }
-- no_ispc input { [0u8] }
-- output { [0x93u8, 0xb8, 0x85, 0xad, 0xfe, 0x0d, 0xa0, 0x89, 0xcd, 0xf6, 0x34, 0x90, 0x4f, 0xd5, 0x9f, 0x71] }

type md5 = (u32, u32, u32, u32)

def us32 (x: i32) = u32.i32 x

def rs: [64]u32 =
  map us32
  [ 7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
    5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
    4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
    6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21 ]

def ks: [64]u32 =
  [ 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee ,
    0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501 ,
    0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be ,
    0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821 ,
    0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa ,
    0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8 ,
    0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed ,
    0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a ,
    0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c ,
    0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70 ,
    0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05 ,
    0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665 ,
    0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039 ,
    0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1 ,
    0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1 ,
    0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391 ]

def rotate_left(x: u32, c: u32): u32 = (x << c) | (x >> (32u32 - c))

def bytes(x: u32): [4]u8 = [u8.u32(x),
                            u8.u32(x/0x100u32),
                            u8.u32(x/0x10000u32),
                            u8.u32(x/0x1000000u32)]

def unbytes(bs: [4]u8): u32 =
  u32.u8(bs[0]) +
  u32.u8(bs[1]) * 0x100u32 +
  u32.u8(bs[2]) * 0x10000u32 +
  u32.u8(bs[3]) * 0x1000000u32

def unbytes_block(block: [16*4]u8): [16]u32 =
  map unbytes (unflatten block)

-- Process 512 bits of the input.
def md5_chunk ((a0,b0,c0,d0): md5) (m: [16]u32): md5 =
  loop (a,b,c,d) = (a0,b0,c0,d0) for i < 64 do
    let (f,g) =
      if      i < 16 then ((b & c) | (!b & d),
                           u32.i32 i)
      else if i < 32 then ((d & b) | (!d & c),
                           (5u32*u32.i32 i + 1u32) % 16u32)
      else if i < 48 then (b ^ c ^ d,
                           (3u32*u32.i32 i + 5u32) % 16u32)
      else                (c ^ (b | !d),
                           (7u32*u32.i32 i)        % 16u32)
    in (d, b + rotate_left(a + f + ks[i] + m[i32.u32 g], rs[i]), b, c)

def md5 [n] (ms: [n][16]u32): md5 =
  let a0 = 0x67452301_u32
  let b0 = 0xefcdab89_u32
  let c0 = 0x98badcfe_u32
  let d0 = 0x10325476_u32
  in loop ((a0,b0,c0,d0)) for i < n do
       let (a,b,c,d) = md5_chunk (a0,b0,c0,d0) ms[i]
       in (a0+a, b0+b, c0+c, d0+d)

def main [n] (ms: [n]u8): [16]u8 =
  let padding = 64 - (n % 64)
  let n_padded = n + padding
  let num_blocks = n_padded / 64
  let ms_padded = ms ++
                  bytes 0x80u32 ++
                  replicate (padding-12) 0x0u8 ++
                  bytes (u32.i64(n*8)) ++
                  [0u8,0u8,0u8,0u8]
                  :> [num_blocks*(16*4)]u8
  let (a,b,c,d) = md5 (map unbytes_block (unflatten ms_padded))
  in flatten (map bytes [a,b,c,d]) :> [16]u8
