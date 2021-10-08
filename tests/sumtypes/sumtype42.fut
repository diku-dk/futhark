-- ==
-- input { -1i32 } output { true }
-- input {  1i32 } output { false }

let main (x: i32) =
  match x case -1 -> true
          case _ -> false
