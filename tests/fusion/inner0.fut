-- #2238
-- ==
-- structure { Screma 2 }

type opt 'a = #none | #some a

def first_some 't (xs: [](opt t)) : opt t =
  reduce (\x y ->
            match (x, y)
            case (#some x, _) -> #some x
            case (_, #some y) -> #some y
            case _ -> #none)
         #none
         xs

def le [n] [m] 't ((<=): t -> t -> bool) (a: [n]t) (b: [m]t) : bool =
  let minlen = i64.min n m
  let (<) = \x y -> x <= y && !(y <= x)
  let cmp =
    map2 (\x y : opt bool ->
            if x < y
            then #some true
            else if y < x
            then #some false
            else #none)
         (take minlen a)
         (take minlen b)
  in match first_some cmp
     case #some res -> res
     case #none -> n i64.<= m

def minimum [n] 't ((<=): t -> t -> bool) (a: [n]t) : t =
  reduce (\x y -> if x <= y then x else y) a[0] a

entry main (A: []i32) (ijs: [](i64, i64)) =
  minimum (\x y -> le (i32.<=) A[x.0:x.1] A[y.0:y.1]) ijs
