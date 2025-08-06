-- Missing pattern warning 3.
-- ==
-- error: Unmatched

type foobar = #foo | #bar

def f : i32 =
  match #foo
  case (#foo: foobar) -> 1
