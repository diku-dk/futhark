-- Invalid return type with overlapping constructor.
-- ==
-- error:

def g (x : #foo | #bar) : #foo =
  match x
    case #foo -> #foo
    case #bar -> #bar
