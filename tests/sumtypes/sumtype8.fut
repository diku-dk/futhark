-- Constructor order shouldn't matter.
-- ==

type foobar = #foo i32 | #bar i32
type barfoo = #bar i32 | #foo i32

let main (x : foobar) = (#bar 5) : barfoo