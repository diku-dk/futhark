-- Sumtype consumption.
-- ==

type^ sum = #foo ([]i32) | #bar ([]i32)

def main (v: *sum) : *[]i32 =
  match v
  case #foo arr -> arr
  case #bar arr -> arr
