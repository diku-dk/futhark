-- ==
-- structure { Apply 1 }

let f (x: i64) = x + 2

let main x =
  map (\i -> #[noinline] f i) (iota x)
