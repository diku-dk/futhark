-- Arrays of enums.
-- ==
-- input { }
-- output { [1, 2, 3, 4] }

type animal = #dog | #cat | #mouse | #bird

let f (x : animal) : i32 = 
  match x
    case #dog   -> 1
    case #cat   -> 2
    case #mouse -> 3
    case #bird  -> 4

let main : []i32 = map f [#dog, #cat, #mouse, #bird]
