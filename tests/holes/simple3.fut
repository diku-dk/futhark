-- ==
-- error: causality

def main (b: bool) (A: []i32) =
  if b then filter (> 0) A else ???
