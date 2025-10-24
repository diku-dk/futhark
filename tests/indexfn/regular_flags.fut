def regular_flags (n: {i64 | \x -> Range x (0,inf)}) (m: {i64 | \x -> Range x (0,inf)}) : {[]i64 | \_ -> true} =
  let xss = map (\i -> map (\j -> if j == 0 then i else 0) (iota m)) (iota n)
  let xs = flatten xss
  in map (\i -> i + 1) xs
