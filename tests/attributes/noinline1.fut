-- ==
-- structure { Apply 1 }

def f (x: i64) = x + 2

def main x =
  map (\i -> #[noinline] f i) (iota x)
