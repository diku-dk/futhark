-- Enum swap constructors in array.
-- ==
-- input { }
-- output { [2, 1] }

let f (x : #foo | #bar) : #foo | #bar =
  match x
    case #foo -> #bar
    case #bar -> #foo

let g (x : #foo | #bar) : i32 =
  match x 
    case #foo -> 1
    case #bar -> 2

let main = map g (map f [#foo, #bar])
