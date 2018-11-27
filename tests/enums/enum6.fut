-- Enums as a type parameter.
-- ==
-- input { } 
-- output { 2 } 

let id 'a (x : a) : a = x

let f (x : #foo | #bar) : i32 = 
  match x
    case #foo   -> 1
    case #bar   -> 2

let main : i32 = f (id #bar)
