def loc (n : i64) (i : i64) (r : i64) : i64 =
  if 0 <= i + r && i + r < n
  then i + r
  else if i + r < 0
       then n + i + r
       else i + r - n

-- TODO index function for mono is wrong; apparently I get that
-- compare is unary and then create two index functions
-- [compare xs[i], compare xs[i+1]]
def mono 't [n] (compare: t -> t -> bool) (xs: [n]t) =
  map (\i -> compare xs[i] xs[i+1]) (iota (n-1))

def mono_icr [n] (xs: [n]i64) =
  map (\i -> xs[i] < xs[i+1]) (iota (n-1))

-- NOTE cannot prove permutation without monotonicity.
def rotate [n] 't (r: {i64 | \j -> 0 <= j && j < n}) (a: [n]t) : [n]t =
  map (\i -> a[loc n i r]) (iota n)
