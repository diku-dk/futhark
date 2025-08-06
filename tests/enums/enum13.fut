-- Comparison of enums in records.
-- ==
-- input { }
-- output { [true, false] }

type rec = {f1: #foo | #bar, f2: #vim | #emacs}

def main =
  let (r: rec) = {f1 = #foo, f2 = #emacs}
  in [r.f1 == #foo, r.f1 == #bar]
