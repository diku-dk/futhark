-- A lambda must not lose the uniqueness of its declared return type when
-- the existential sizes of that type are inferred.
-- ==
-- error: aliased to "ys"

def main (xs: []i32) = (\(k: i64, ys: []i32) : (*[]i32) -> ys) (1, xs)
