-- Prelude
def sum [n] (xs: [n]i64) =
  if n > 0 then (scan (+) 0 xs)[n-1] else 0

def to_i64 c : i64 = if c then 1 else 0


def filter_indices [n]
  (cs: [n]bool)
  (xs: [n]f32)
  : {(i64, [n]i64) | \(m, is) ->
      let correct_size = m == sum (map (\x -> to_i64 x) cs)
      let no_dups = injectiveRCD (0, m-1) is
      let in_range = map2 (\c i -> if c then 0 <= i && i < m else true) cs is
      -- m is the correct size and is is a permutation of 0 .. m:
      in correct_size && no_dups && and in_range
    } =
  let num_trues = scan (+) 0 (map (\c -> to_i64 c) cs)
  let new_size = if n > 0 then num_trues[n-1] else 0
  let is = map2 (\c i -> if c then i-1 else -1) cs num_trues
  in (new_size, is)

def filter [n] (p: f32 -> bool) (xs: [n]f32) : {[]f32 | \_ -> true} =
  let cs = map (\x -> p x) xs
  let (new_n, is) = filter_indices cs xs
  let scratch = replicate new_n 0f32
  in scatter scratch is xs
