-- Missing pattern warning 4; intended behaviour is to print the warning without
-- superfluous parentheses.
-- ==
-- error: Unmatched

type foobar = #foo | #bar

def f : i32 =
  match #foo : foobar
  case (((((#foo))))) -> 1
