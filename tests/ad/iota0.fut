-- ==
-- tags { autodiff }
-- entry: rev
-- compiled input { 5i64 }
-- output { 5i64 }

def f (n: i64) = i64.sum (iota n)

entry rev n = vjp f n 1
