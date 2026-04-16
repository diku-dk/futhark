-- | A sequential implementation of insertion sort.

local
def swap 't [n] (i: i64) (j: i64) (xs: *[n]t) : *[n]t =
  -- Need copies to prevent the uniqueness checker from getting
  -- cranky.
  let xi = copy xs[i]
  let xs[i] = copy xs[j]
  let xs[j] = xi
  in xs

-- | Insertion sort.  Runs with *O(n^2)* work and *O(n^2)* depth.
def insertion_sort [n] 't ((<=): t -> t -> bool) (xs: [n]t) : *[n]t =
  -- Make a copy of the array so we can operate in-place.
  loop xs = copy xs
  for i in 1..<i64.max n 1 do
    -- Construct our own greather-than function out of <=.
    let gt x y = !(x <= y)
    let (_, xs') =
      loop (j, xs) = (i, xs)
      while j > 0 && (xs[j - 1] `gt` xs[j]) do
        (j - 1, swap j (j - 1) xs)
    in xs'

-- | Like `insertion_sort`, but sort based on key function.
def insertion_sort_by_key [n] 't 'k (key: t -> k) ((<=): k -> k -> bool) (xs: [n]t) : [n]t =
  zip (map key xs) (iota n)
  |> insertion_sort (\(x, _) (y, _) -> x <= y)
  |> map (\(_, i) -> xs[i])
