-- Enum swap constructors in array.
-- ==
-- input { }
-- output { [2, 1] }

def f (x: #foo | #bar) : #foo | #bar =
  match x
  case #foo -> #bar
  case #bar -> #foo

def g (x: #foo | #bar) : i32 =
  match x
  case #foo -> 1
  case #bar -> 2

def main = map g (map f [#foo, #bar])
