-- Ambiguous enums should yield an error.
-- ==
-- error:

def f : bool =
  match #foo
  case #foo -> true
  case #bar -> true
