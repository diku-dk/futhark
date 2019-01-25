type dir = #up | #down

let move (x: i32) (d: dir) =
  match d
  case #down -> x+1
  case #up -> x-1

let main x = (move x #up, move x #down)
