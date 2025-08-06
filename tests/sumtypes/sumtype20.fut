-- Missing pattern warning error.
-- ==
-- error: Unmatched cases

type some = #none | #some i32 i32
type foobar = #foo i32 some i32

def main : i32 =
  --   match (#foo 1 (#some 2) 3) : foobar
  --   case (#foo 1 (#some 2) 3) -> 1
  match (#some 1 2) : some
  case (#none) -> 1
  case (#some 1 2) -> 2
