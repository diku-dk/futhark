def main (b: bool) (i: i64) (xs: []i64) =
  #[unsafe]
  if b then (iota xs[i + 1], xs[0::2]) else (xs[0::2], iota xs[i + 1])
