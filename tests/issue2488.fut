-- Hole with function type in let-defined function should not crash (#2488).
-- ==
-- warning: Hole

def foo =
  let f _ = ???
  in f true false
