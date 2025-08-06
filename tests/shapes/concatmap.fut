-- ==
-- input { [1i64,2i64,3i64] } output { [0i64,0i64,1i64,0i64,1i64,2i64] }

def concatmap [n] 'a 'b (f: a -> []b) (as: [n]a) : []b =
  loop acc = []
  for a in as do
    acc ++ f a

def main (xs: []i64) = concatmap iota xs
