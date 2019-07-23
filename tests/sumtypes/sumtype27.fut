-- ==
-- input { [0,-1,3,4,-2] }
-- output { 1 }

type opt 'a = #some a | #none

let opt 'a 'b (b: b) (f: a -> b) (x: opt a) : b =
  match x case #some x' -> f x'
          case #none -> b

let cat_opt [n] (xs: [n](opt i32)) =
  let either (x : opt i32) (y : opt i32) = opt x (\y' -> #some y') y
  in match reduce either #none xs
     case #none -> 0
     case #some _ -> 1

let main (xs: []i32) = cat_opt (map (\x -> #some x) xs)
