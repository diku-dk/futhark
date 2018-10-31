-- Basic enum types and matches.
-- ==
-- input { } 
-- output { 5 }

type animal = #dog | #cat | #mouse | #bird

let main : i32 =
  match #mouse : animal
    case #dog   -> 6
    case #bird  -> 9
    case #mouse -> 5
    case #cat   -> 0
