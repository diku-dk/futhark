-- ==
-- input { [1,2,-1,3] }
-- output { [-1] }

type^ opt '~a = #some a

def map_opt 'a '~b (f: a -> b) (a: opt a) : opt b =
  match a
  case #some a' -> (#some (f a') : opt b)

entry main l =
  match map_opt (filter (< 0i32)) (#some l)
  case #some l' -> l'
