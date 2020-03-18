type^ myType = #myVal (i32 -> i32)

let main =
  match ((\m   -> #myVal (\_ -> m)) 0 : myType)
  case #myVal m -> m 1
