-- Missing pattern warning 10.
-- (Checks that warnings are still triggered with ambiguous types)
-- ==
-- error:

type foobar = #foo | #bar

def f : bool =
  match (true, 10, {f1 = #foo : foobar, f2 = 1.2})
  case (true, 10, {f1 = #foo, f2 = 1.2}) -> true
