-- Test that size constraints on opaque types are respected.

type vec [n] = {vec: [n]i64}

entry mk_vec (n: i64) : vec [n] = {vec=iota n}

entry use_vec [n] (x: vec [n]) (y: vec [n]) = i64.sum (map2 (+) x.vec y.vec)
