-- Existential sizes must not be (exclusively) used as a parameter
-- type.
-- ==
-- error: Unknown size.*in parameter

def f (x: bool) =
  let n = if x then 10 else 20
  in \(_: [n]bool) -> true
