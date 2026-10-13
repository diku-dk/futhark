-- As lambda-return3.fut, but where the lambda also declares its
-- existential return type.  The declared quantifier is solved to the
-- size hidden by the parameter pattern, so it must be re-quantified.
-- ==
-- input { [1i64,2i64,0i64] [1,2,3] } output { [1i64,2i64,0i64] }

def sizes [n] 'a 'b (f: a -> ?[k].[k]b) (as: [n]a) : [n]i64 =
  map (\a -> length (f a)) as

def main (ks: []i64) (xs: []i32) =
  sizes (\(k: i64, x: i32) : (?[m].[m]i32) -> replicate k x) (zip ks xs)
