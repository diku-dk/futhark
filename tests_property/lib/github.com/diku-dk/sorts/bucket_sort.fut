-- | Module with bucket sort implementation.
--
-- Well suited for cases where the number of buckets is small or a
-- specific asymptotic property is needed to be fulfilled by your
-- program like linear work with a super-constant amount of keys, i.e.
-- `num_buckets = f(n)` is not in *O(1)*.

local
module mk_bucket_sort (I: integral) = {
  def sort 'a [n] (num_buckets: i64) (get_bucket: a -> i64) (vs: [n]a) : [n]a =
    let block_size = num_buckets
    let block_num = (n + block_size - 1) / block_size
    let block_count block_i =
      let start = block_i * block_size
      let end = i64.min n ((block_i + 1) * block_size)
      let rank = replicate block_size (I.i64 0)
      let count = replicate block_size 0i64
      in loop (rank, count)
         for j < end - start do
           let i = get_bucket vs[start + j]
           let rank[j] = I.i64 count[i]
           let count[i] = count[i] + 1
           in (rank, count)
    let (ranks, counts) =
      tabulate block_num block_count
      |> unzip
    let counts = transpose counts |> flatten
    let offsets =
      counts
      |> scan (+) 0
      |> flip (map2 (-)) counts
      |> unflatten
      |> transpose
    let flat_ranks = flatten ranks
    let is =
      tabulate n (\i ->
                    let block_id = i / block_size
                    let j = get_bucket vs[i]
                    in offsets[block_id][j] + I.to_i64 flat_ranks[i])
    let sorted_values = scatter (#[scratch] replicate n vs[0]) is vs
    in sorted_values
}

local module bucket_sort_max8bit_module = mk_bucket_sort u8
local module bucket_sort_max16bit_module = mk_bucket_sort u16
local module bucket_sort_max32bit_module = mk_bucket_sort u32
local module bucket_sort_max63bit_module = mk_bucket_sort i64

-- | Implementation of bucket sort where `num_buckets` is the number
-- of buckets and `get_bucket` is a function which maps a element to a
-- integer in a contiguous interval of integers from 0 to
-- `num_buckets` - 1. The function uses dynamic dispatching to select
-- different bucket sorts algorithms depending on the number of
-- buckets to save memory. To avoid excessive code genereration use
-- the specialised bucket sort.
--
-- **Work:** *O(n ✕ W(get_bucket))*
--
-- **Span:** *O(num_buckets ✕ W(get_bucket) + log n)*
def bucket_sort 'k [n] (num_buckets: i64) (get_bucket: k -> i64) (xs: [n]k) : [n]k =
  if num_buckets <= 1i64 + i64.u8 u8.highest
  then bucket_sort_max8bit_module.sort num_buckets get_bucket xs
  else if num_buckets <= 1i64 + i64.u16 u16.highest
  then bucket_sort_max16bit_module.sort num_buckets get_bucket xs
  else if num_buckets <= 1i64 + i64.u32 u32.highest
  then bucket_sort_max32bit_module.sort num_buckets get_bucket xs
  else bucket_sort_max63bit_module.sort num_buckets get_bucket xs

-- | Bucket sort with maximally 256 buckets.
def bucket_sort_max8bit 'k [n] (num_buckets: i64) (get_bucket: k -> i64) (xs: [n]k) : [n]k =
  bucket_sort_max8bit_module.sort num_buckets get_bucket xs

-- | Bucket sort with maximally 65536 buckets.
def bucket_sort_max16bit 'k [n] (num_buckets: i64) (get_bucket: k -> i64) (xs: [n]k) : [n]k =
  bucket_sort_max16bit_module.sort num_buckets get_bucket xs

-- | Bucket sort with maximally 4294967296 buckets, you probably
-- should never use this.
def bucket_sort_max32bit 'k [n] (num_buckets: i64) (get_bucket: k -> i64) (xs: [n]k) : [n]k =
  bucket_sort_max32bit_module.sort num_buckets get_bucket xs

-- | Bucket sort with maximally 9223372036854775808 buckets, you
-- probably should never use this.
def bucket_sort_max63bit 'k [n] (num_buckets: i64) (get_bucket: k -> i64) (xs: [n]k) : [n]k =
  bucket_sort_max63bit_module.sort num_buckets get_bucket xs
