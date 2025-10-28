-- Constructor order shouldn't matter.
-- ==

type foobar = #foo | #bar
type barfoo = #bar | #foo

def main (x: foobar) = #bar : barfoo
