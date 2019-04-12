-- Missing pattern warning 2.
-- ==
-- input { }
-- output { 5 }

type foobar = #foo (i32, (i32, i32), i32) | #bar
type moo    = #moo i32 foobar i32
type boo    = #boo (i32, moo)
type blah   = #blah i32

let main : i32 =
      match (#boo (7, #moo 5 (#foo (1, (2, 3), 4)) 6)) : boo
      case (#boo (_, (#moo _ (#foo (_, (_, 3), _)) 6))) -> 2
      case (#boo (_, (#moo _ (#bar )))) -> 2