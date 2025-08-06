-- Sizes obtained with existantialy bounded sizes should not be calculated
-- ==
-- input { 2i64 } output { [0i64, 1i64, 0i64, 1i64] }

def double_eval 't (f: () -> []t) : []t =
  f () ++ f ()

def main (n: i64) : []i64 =
  double_eval (\_ -> iota n)
