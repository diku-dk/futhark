-- Existential sizes must not be (exclusively) used as a parameter
-- type.
-- ==
-- error: Unknown size.*in parameter

def f (x: bool) =
  let (n, _) = if x then (10, true) else (20, true)
  in \(_: [n]bool) -> true
