type opaque = ([10]i64, bool)

entry mk_opaque b : opaque = (iota 10, b)

entry unmk_opaque ((a, b): opaque) = (b, a)
