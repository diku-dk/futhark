-- | Parallel bubble sort.
--
-- This may be useful if you have almost-sorted data that you want to
-- make fully-sorted in parallel.  Obviously *very* slow for
-- non-sorted data.

-- | Parallel bubble sort.  Runs with *O(n^2)* work and *O(n^2)* depth.
def bubble_sort [n] 't ((<=): t -> t -> bool) (xs: [n]t) : [n]t =
  let f b xs i =
    let dir = if i % 2 == 0 then b else -b
    let j = i + dir
    let cmp x y =
      if dir == 1
      then x <= y
      else !(x <= y)
    in if j >= 0 && j < n && (xs[j] `cmp` xs[i])
       then (true, xs[j])
       else (false, xs[i])
  let iter xs b =
    let (changed, xs) = tabulate n (f b xs) |> unzip
    in (xs, -b, or changed)
  in (loop (xs, b, continue) = (xs, 1, true) while continue do iter xs b).0

-- | Like `bubble_sort`@term, but sort based on key function.
def bubble_sort_by_key [n] 't 'k (key: t -> k) ((<=): k -> k -> bool) (xs: [n]t) : [n]t =
  zip (map key xs) (iota n)
  |> bubble_sort (\(x, _) (y, _) -> x <= y)
  |> map (\(_, i) -> xs[i])
