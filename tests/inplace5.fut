-- In-place update of the middle of an array.
-- ==
-- input { [0u8,1u8,2u8,3u8,4u8] 1i64 3i64 }
-- output { [1u8, 2u8, 3u8, 128u8, 1u8, 2u8, 3u8, 0u8] }

def main (bs: []u8) i k =
  let k = i64.min 8 k
  let one_bit = [128u8, 1u8, 2u8, 3u8]
  let block = replicate 8 0u8
  let block[0:k] = bs[i:i + k]
  let block[k:k + 4] = one_bit
  in block
