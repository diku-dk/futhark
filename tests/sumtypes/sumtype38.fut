-- ==
-- error: Unmatched

type r = {f0: bool, f1: bool}

def f (x: r) =
  match x
  case {f0 = false, f1 = false} -> 0
  case {f0 = true, f1 = true} -> 0
