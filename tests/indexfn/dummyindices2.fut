def dummyindices [n] (conds: [n]bool) : [n]i64 =
  map (\ c -> n-1) conds
