-- Library with a simple tuple type to test record projection.

type pair = (i32, i64)

entry mk_pair (x: i32) (y: i64) : pair = (x, y)
