-- Missing pattern warning 5 (integers).
-- ==
-- error:

let f : i32 =
  match (1 : i32)
    case 1 -> 1
    case 2 -> 2
