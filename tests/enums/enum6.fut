-- Enums as a type parameter.
-- ==
-- input { }
-- output { 2 }

def id 'a (x: a) : a = x

def f (x: #foo | #bar) : i32 =
  match x
  case #foo -> 1
  case #bar -> 2

def main : i32 = f (id #bar)
