def with_indices [n] 'a (xs: [n]a) : [n](a, i64) =
  zip xs (iota n)

def get_rbit [n] (a: i32) (r: [2 * n]i8) : i32 =
  if i64.i32 a >= n
  then 0
  else i32.i8 r[(i64.i32 a) + n]

local
def radix_sort_step [n] 't
                    (xs: [n]t)
                    (get_bit: i32 -> t -> i32)
                    (digit_n: i32) : [n]t =
  let num x = get_bit (digit_n + 1) x * 2 + get_bit digit_n x
  let pairwise op (a1, b1, c1, d1) (a2, b2, c2, d2) = (a1 `op` a2, b1 `op` b2, c1 `op` c2, d1 `op` d2)
  let bins = xs |> map num
  let flags =
    bins
    |> map (\x ->
              ( i64.bool (x == 0)
              , i64.bool (x == 1)
              , i64.bool (x == 2)
              , i64.bool (x == 3)
              ))
  let offsets = scan (pairwise (+)) (0, 0, 0, 0) flags
  let (na, nb, nc, _nd) = last offsets
  let f bin (a, b, c, d) =
    (-1)
    + a * (i64.bool (bin == 0))
    + na * (i64.bool (bin > 0))
    + b * (i64.bool (bin == 1))
    + nb * (i64.bool (bin > 1))
    + c * (i64.bool (bin == 2))
    + nc * (i64.bool (bin > 2))
    + d * (i64.bool (bin == 3))
  let is = map2 f bins offsets
  in scatter (copy xs) is xs

def radix_sort [n] 't
               (num_bits: i32)
               (get_bit: i32 -> t -> i32)
               (xs: [n]t) : [n]t =
  let iters = if n == 0 then 0 else (num_bits + 2 - 1) / 2
  in loop xs for i < iters do radix_sort_step xs get_bit (i * 2)

local
def by_key_wrapper [n] 't sorter key num_bits get_bit (xs: [n]t) : [n]t =
  map key xs
  |> with_indices
  |> sorter num_bits (\i (k, _) -> get_bit i k)
  |> map (\(_, i: i64) -> xs[i])

def radix_sort_by_key [n] 't 'k
                      (key: t -> k)
                      (num_bits: i32)
                      (get_bit: i32 -> k -> i32)
                      (xs: [n]t) : [n]t =
  by_key_wrapper radix_sort key num_bits get_bit xs

def main =
  let n = 5
  let m = 3
  let r = replicate m (replicate (2 * n) 0i8)
  let amp = replicate m 1
  in radix_sort_by_key (.0) (i32.i64 n) get_rbit (zip r amp)
