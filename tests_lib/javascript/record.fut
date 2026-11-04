-- Records and tuples, which are opaque, and whose fields are projected.

type pair = (i32, i64)

entry mk_pair (x: i32) (y: i64) : pair = (x, y)

-- The array type of the field is used only inside the record.
type~ rec = {xs: []f32, n: i32}

entry mk_rec (n: i64) : rec = {xs = map f32.i64 (iota n), n = i32.i64 n}

entry sum_rec (r: rec) : f32 = f32.sum r.xs + f32.i32 r.n

-- An entry point that returns a tuple returns a single opaque value.
entry tup (n: i64) : (i32, []i64) = (i32.i64 n, iota n)
