-- Sumtype equality.
-- ==
-- error: use pattern matching

type foobar = #foo i32 | #bar i32

def main : i32 = if ((#foo 5) : foobar) == #foo 4
                 then 1
                 else if ((#bar 1) : foobar) == #bar 1
                      then 2
                      else 3
