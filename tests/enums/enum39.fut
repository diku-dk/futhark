-- Missing pattern warning 9.
-- ==
-- error: Unmatched

type foobar = #foo | #bar
type rec = {f1: foobar, f2: f32}

def f : bool =
  match (true, 10 : i32, {f1 = #foo, f2 = 1.2} : rec)
  case (true, 10, {f1 = #foo, f2 = 1.2}) -> true
