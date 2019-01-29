-- Sumtype as a type parameter.
-- ==
-- input { } 
-- output { 2 } 

let id 'a (x : a) : a = x

let f (x : #foo i32 | #bar i32) : i32 = 
  match x
    case (#foo y)  -> y
    case (#bar y)  -> y

let main : i32 = f (id (#bar 2))