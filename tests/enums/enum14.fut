-- Constructor order shouldn't matter.
-- ==

type foobar = #foo | #bar
type barfoo = #bar | #foo

let main (x : foobar) = #bar : barfoo
