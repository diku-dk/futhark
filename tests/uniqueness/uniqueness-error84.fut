-- The payload of a constructor is treated as a tuple, so a sum whose
-- payload components alias each other cannot be fresh.
-- ==
-- error: aliased to some other component

type~ t = #foo ([]i32) ([]i32) | #bar

def f (xs: *[]i32) : *t = #foo xs xs
