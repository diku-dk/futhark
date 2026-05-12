-- Size expression should be non-consuming
-- ==
-- error: "ns".*not consumable
def consume (xs: *[]i64) : i64 = xs[0]
def f [n] (ns: *[n]i64) (xs: [consume ns]f32) = xs[0]
