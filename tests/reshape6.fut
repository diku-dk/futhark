-- A flatten followed by an unflatten that only consumes some of the
-- produced dimensions is not an inverse; the trailing unit dimension
-- must be preserved.
-- ==
-- input { [[0i64, 1i64], [2i64, 3i64]] }
-- output { [[[0i64], [1i64]], [[2i64], [3i64]]] }

entry main (x: [2][2]i64) =
  let y = unflatten (flatten x :> [4 * 1]i64) :> [4][1]i64
  in unflatten (y :> [2 * 2][1]i64) :> [2][2][1]i64
