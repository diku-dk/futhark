-- ==
-- structure { Apply 1 }

def f (x: i32) = x + 2

def main x =
  #[noinline] f x
