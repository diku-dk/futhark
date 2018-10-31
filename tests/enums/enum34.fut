-- Missing pattern warning 4; intended behaviour is to print the warning without
-- superfluous parentheses.
-- ==
-- warning: unmatched

type foobar = #foo | #bar

let f : i32 =
  match #foo : foobar
    case (((((#foo))))) -> 1
