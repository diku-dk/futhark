-- Missing pattern warning 3.
-- ==
-- warning: unmatched

type foobar = #foo | #bar

let f : i32 =
  match #foo
    case (#foo : foobar) -> 1
