let main is js =
  [let n = 0 in map2(\i j -> (iota 5 with [i] = j)[n]) is js
  ,let n = 1 in map2(\i j -> (iota 5 with [i] = j)[n]) is js
  ,let n = 2 in map2(\i j -> (iota 5 with [i] = j)[n]) is js
  ,let n = 3 in map2(\i j -> (iota 5 with [i] = j)[n]) is js
  ,let n = 4 in map2(\i j -> (iota 5 with [i] = j)[n]) is js
  ]
