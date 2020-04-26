-- ==
-- input { [1,2,3] } output { [0,0,1,0,1,2] }

let concatmap [n] 'a 'b (f: a -> []b) (as: [n]a) : []b =
  loop acc = [] for a in as do
    acc ++ f a

let main (xs: []i32) = concatmap iota xs
