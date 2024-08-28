entry main (x: i32) (y: i32): bool =
  match x
  case x' -> (\z -> z == x') y
