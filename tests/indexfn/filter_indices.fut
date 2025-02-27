-- Prelude
def sum [n] (xs: [n]i64) =
  if n > 0 then (scan (+) 0 xs)[n-1] else 0

def to_i64 c : i64 = if c then 1 else 0


def filter_indices [n]
  (p: f32 -> bool)
  (xs: [n]f32)
  : {(i64, [n]i64) | \(m, is) ->
      let correct_size = m == sum (map (\x -> to_i64 (p x)) xs)
      let no_dups = injectiveOn (0, m-1) is
      let in_range = map2 (\x i -> if p x then 0 <= i && i < m else true) xs is
      -- m is the correct size and is is a permutation of 0 .. m:
      in correct_size && no_dups && and in_range
    } =
  let cs = map (\x -> p x) xs
  let num_trues = scan (+) 0 (map (\c -> to_i64 c) cs)
  let new_size = if n > 0 then num_trues[n-1] else 0
  let is = map2 (\c i -> if c then i-1 else -1) cs num_trues
  in (new_size, is)
