-- Test that we can store and restore opaques.

type whatever [n] = {vec: [n](i64, bool), b:bool}

entry mk (b: bool) : whatever []  = {vec = [(1,b),(2, !b)], b}

entry unmk ({vec, b}: whatever []) : ([]i64, []bool, bool) =
  let (xs,ys) = unzip vec
  in (xs, ys, b)
