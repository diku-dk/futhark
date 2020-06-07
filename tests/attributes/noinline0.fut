-- ==
-- structure { Apply 1 }

let f (x: i32) = x + 2

let main x =
  #[noinline] f x
