-- Local ambiguous enum.
-- ==
-- input { }
-- output { [2,2] }

let f (x : #foo | #bar) : [](#foo | #bar) = 
  let id 't (x : t) : t = x 
  in match id x 
      case #foo -> [#foo, #foo]
      case #bar -> [#bar, #bar]

let g (x : #foo | #bar) : i32 = 
  match x
    case #foo -> 1
    case #bar -> 2

let main : []i32 = map g (f #bar)