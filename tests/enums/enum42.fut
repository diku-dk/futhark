-- Ambiguous enums should yield an error.
-- ==
-- error:

let f : bool =
  match #foo
  case #foo -> true
  case #bar -> true
