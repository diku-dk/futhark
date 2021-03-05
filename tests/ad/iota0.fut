-- ==
-- entry: rev
-- compiled input { 5i64 }
-- output { 5i64 }

let f (n: i64) = i64.sum (iota n)

entry rev n = vjp f n 1
