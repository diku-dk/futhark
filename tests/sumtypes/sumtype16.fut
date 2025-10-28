-- Type abbreviations
-- ==
-- input { }
-- output { 1 }

type foobar 't = #foo t | #bar i32

def main : i32 =
  match (#foo 1) : foobar i32
  case (#foo x) -> x
  case _ -> 0
