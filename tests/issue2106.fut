-- ==
-- input { 2i64 }
-- output { [0i64, 0i64] }

def f'' 'a (n: i64) (f: a -> i64) (a: a): [n]i64 = replicate n (f a)

def f (s: {n: i64}): [s.n]i64 =
  let f' 'a (f: a -> i64) (a: a): [s.n]i64 = f'' s.n f a
  in f' id 0

entry main (n: i64) = f {n}
