-- ==
-- input { -1f32 } output { true }
-- input {  1f32 } output { false }

let main (x: f32) =
  match x case -1f32 -> true
          case _ -> false
