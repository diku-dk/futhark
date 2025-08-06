-- Tests that the code generator does not choke on terrible names that
-- are not valid in C.
--
-- ==
-- input { false 2 }
-- output { 17 }

def main (r': bool) (x_: i32) : i32 =
  if r' then 123 else (x_ + 1) * 2 + 6 + (loop x = 1 for i' < x_ do (x << 1) ^ i')
