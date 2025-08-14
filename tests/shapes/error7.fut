-- Ambiguous size of sum type.
-- ==
-- error: Ambiguous.*anonymous size in type expression

type~ sum = #foo ([]i32) | #bar ([]i32)

def main (xs: *[]i32) =
  let v: sum = #foo xs
  in xs
