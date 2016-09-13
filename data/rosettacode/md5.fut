-- MD5 implementation.  Based on Accelerate and other sources.
--
-- Easy to get wrong if you forget that MD5 is little-endian.
--
-- ==
-- input { empty(i32) }
-- output { 0xd98c1dd4 0x4b2008f 0x980980e9 0x7e42f8ec }

type md5 = (u32, u32, u32, u32)

fun rs(): [64]u32 =
  map u32
  ([ 7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
     5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
     4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
     6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21 ])

fun ks(): [64]u32 =
  map u32
  ([ 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee ,
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
     0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391 ])

fun md5(ms: [n][16]u32): md5 =
  let a0 = u32(0x67452301)
  let b0 = u32(0xefcdab89)
  let c0 = u32(0x98badcfe)
  let d0 = u32(0x10325476)
  loop ((a0,b0,c0,d0)) = for i < n do
    let (a,b,c,d) = md5_chunk (a0,b0,c0,d0) ms[i]
    in (a0+a, b0+b, c0+c, d0+d)
  in (a0,b0,c0,d0)

fun rotate_left(x: u32, c: u32): u32 = (x << c) | (x >> (32u32 - c))

fun main(ms: [n]u32): md5 =
  let padding = 16 - 1 - (n % 16)
  let n_padded = n + 1 + padding
  let ms_padded = concat ms ([0x80u32]) (replicate padding 0x0u32)
  in md5 (reshape (n_padded / 16, 16) ms_padded)

-- Process 512 bits of the input.
fun md5_chunk ((a0,b0,c0,d0): md5) (m: [16]u32): md5 =
  loop ((a,b,c,d) = (a0,b0,c0,d0)) = for i < 64 do
    let (f,g) =
      if      i < 16 then ((b & c) | ((~b) & d),
                           i)
      else if i < 32 then ((d & b) | ((~d) & c),
                           i32((5u32*u32(i) + 1u32) % 16u32))
      else if i < 48 then (b ^ c ^ d,
                           i32((3u32*u32(i) + 5u32) % 16u32))
      else                (c ^ (b | (~d)),
                           i32((7u32*u32(i))        % 16u32))
    in (d, b + rotate_left(a + f + (ks())[i] + unsafe m[g], (rs())[i]), b, c)
  in (a,b,c,d)
