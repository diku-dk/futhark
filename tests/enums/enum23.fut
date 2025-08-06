-- Matches on records.
-- ==
-- input { }
-- output { 12 }

type foobar = {foo: i32, bar: i32}

def main : i32 =
  match ({foo = 1, bar = 2} : foobar)
  case {foo = 3, bar = 4} -> 9
  case {foo = 5, bar = 6} -> 10
  case {foo = 7, bar = 8} -> 11
  case {foo = 1, bar = 2} -> 12
  case _ -> 12
