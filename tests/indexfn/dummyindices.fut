def dummyindices [n] (conds: [n]bool) : [n]i64 =
  map (\ c -> if c then n-1 else 0) conds
