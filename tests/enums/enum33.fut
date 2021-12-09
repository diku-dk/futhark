-- Missing pattern warning 3.
-- ==
-- error:

type foobar = #foo | #bar

def f : i32 =
  match #foo
    case (#foo : foobar) -> 1
