-- Comparison of enums of different types in records.
-- ==
-- error:

type rec = {f1: #foo | #bar, f2: #vim | #emacs}

def main =
  let (r: rec) = {f1 = #foo, f2 = #emacs}
  in r.f1 == r.f2
