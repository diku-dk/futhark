def dummyindices [n] (conds: [n]bool) : {[n]i64 | \_ -> true} =
  map (\ c -> n-1) conds
