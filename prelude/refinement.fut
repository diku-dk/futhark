def injectiveRCD 't ((a, b) : (i64, i64)) (_: []t) : bool = ???

def bijectiveRCD 't ((a, b) : (i64, i64)) ((c, d) : (i64, i64)) (_: []t) : bool = ???

-- import "soacs"
-- import "functional"
-- import "array"
-- 
-- def injective [n] (xs: [n]i64) =
--   and (map (\i -> xs[i] != xs[i+1]) (iota (n - 1)))
