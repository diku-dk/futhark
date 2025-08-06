-- Matches on records 3.
-- ==
-- input { }
-- output { 3 }

type foopdoop = #foop | #doop
type editors = #emacs | #vim
type foobar = {foo: foopdoop, bar: editors}

def main : i32 =
  match ({foo = #foop, bar = #vim} : foobar)
  case {foo = #doop, bar = #vim} -> 1
  case {foo = #foop, bar = #emacs} -> 2
  case {foo = _, bar = #vim} -> 3
  case {foo = #doop, bar = #emacs} -> 4
