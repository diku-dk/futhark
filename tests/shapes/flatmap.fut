-- ==
-- input { [1,2] } output { [1,2,2,3] }

let flatmap [n] [m] 'a 'b (f: a -> [m]b) (as: [n]a) : []b =
  flatten (map f as)

let main (xs: []i32) = flatmap (\x -> [x,x+1]) xs
