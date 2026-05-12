-- This file is *intentionally* written with DOS linebreaks (\r\n).
-- Don't change it to Unix linebreaks (\n)!
-- ==
-- input { [1,2,3] 4 }
-- error: Index \[4\] out of bounds

def main (a: []i32) (i: i32) : i32 =
  a[i]
