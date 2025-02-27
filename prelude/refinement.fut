def injectivePreimage 't ((a, b) : (i64, i64)) (_: []t) : bool = ???

def injective [n] (xs: [n]i64) =
  injectivePreimage (0,n) xs

-- import "soacs"
-- import "functional"
-- import "array"
-- 
-- def injective [n] (xs: [n]i64) =
--   and (map (\i -> xs[i] != xs[i+1]) (iota (n - 1)))
