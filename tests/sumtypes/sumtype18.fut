-- Missing pattern error.
--
-- Note that there are multiple ways this error can be reported, so if
-- you are fiddling with the match checker, feel free to change the
-- expected error here.
-- ==
-- error: Unmatched cases.*#foo \(#moo _ #none\)

type some 't = #none | #some t
type^ mooboo '^t = #moo t (some i32) | #boo
type^ foobar = #foo (mooboo (i32 -> i32)) | #bar (i32 -> i32)

def main : i32 =
  match (#foo (#moo (+ 1) #none)) : foobar
  case #bar f -> 0
  case #foo #boo -> 2
  case #foo (#moo f (#some 5)) -> 2
