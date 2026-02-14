-- A redomap where part of the result is not reduced.
-- ==
-- input { [5i64,7i64] [0i64,1i64] }
-- output { [20i64, 35i64] [0i64, 1i64] }

def main ns is = map2 (\n (i:i64) -> let is = iota n
                                     let xs = map (+2) is
                                     let ys = map (*i) is
                                     in (i64.sum xs, (opaque ys)[i]))
                      ns is
                 |> unzip
