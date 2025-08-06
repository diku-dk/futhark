-- Sumtype as a type parameter.
-- ==
-- input { }
-- output { 2 }

def id 'a (x: a) : a = x

def f (x: #foo i32 | #bar i32) : i32 =
  match x
  case (#foo y) -> y
  case (#bar y) -> y

def main : i32 = f (id (#bar 2))
