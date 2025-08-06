-- Spurious size annotations maintained by defunctionaliser.
-- ==

def get xs (i: i64) = xs[i]

def test (xs: []i64) (l: i64) : [l]i64 =
  let get_at xs indices = map (get xs) indices
  in get_at xs (iota l)

def main = test (iota 4) 2
