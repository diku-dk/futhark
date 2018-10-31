-- Missing pattern warning 7 (floats).
-- ==
-- warning: unmatched

let f : i32 =
  match {foo = (3.6 : f32), bar = (1 : i32)}
    case {foo = 3, bar = y} -> y
