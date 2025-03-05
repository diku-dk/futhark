-- Prelude
def sum [n] (xs: [n]i64) =
  if n > 0 then (scan (+) 0 xs)[n-1] else 0

def to_i64 c : i64 = if c then 1 else 0

-- def filteredInv [n] (cs: [n]bool) (is: [n]i64) size =
--   let correct_size = size == sum (map (\x -> to_i64 x) cs)
--   let no_dups = bijectiveRCD (0, size-1) (0, size-1) is
--   let part = and (map2 (\c ind -> if c then ind < size else 0 > ind || ind >= size) cs is)
--   in correct_size && no_dups && part

def filter_indices [n]
  (cs: [n]bool)
  : {(i64, [n]i64) | \(m, is) ->
      filteredInv cs is m
    } =
  let num_trues = scan (+) 0 (map (\c -> to_i64 c) cs)
  let new_size = if n > 0 then num_trues[n-1] else 0
  let is = map2 (\c i -> if c then i-1 else -1) cs num_trues
  in (new_size, is)

def filter [n] (p: f32 -> bool) (xs: [n]f32) : {[]f32 | \_ -> true} =
  let cs = map (\x -> p x) xs
  let (new_n, is) = filter_indices cs
  let scratch = replicate new_n 0f32
  in scatter scratch is xs
