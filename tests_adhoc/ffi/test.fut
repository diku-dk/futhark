type r =
  { x: i32
  , y: i32
  }

type s = #a i32 | #b i32 | #c i32

entry p1 : i32 = 1i32
entry p2 : i32 = 2i32
entry p3 : i32 = 3i32

entry r1 : r = {x = 1i32, y = 2i32}
entry r2 : r = {x = 3i32, y = 4i32}
entry r3 : r = {x = 5i32, y = 6i32}

entry s1 : s = #a 2i32
entry s2 : s = #b 4i32
entry s3 : s = #c 6i32

entry pa1 : [3]i32 = [p1, p2, p3]
entry pa2 : [2][3]i32 = [[p1, p2, p3], [p3, p2, p1]]

-- Deliberately not square, so that mistaking one dimension for another
-- is observable.
entry pa3 : [3][2]i32 = [[p1, p2], [p2, p3], [p3, p1]]

entry ra1 : [3]r = [r1, r2, r3]
entry ra2 : [2][3]r = [[r1, r2, r3], [r3, r2, r1]]

entry sa1 : [3]s = [s1, s2, s3]
entry sa2 : [2][3]s = [[s1, s2, s3], [s3, s2, s1]]

-- Floating-point arrays, for differentiation.
entry fa1 : [3]f64 = [1, 2, 3]
entry fa2 : [2][3]f64 = [[1, 2, 3], [4, 5, 6]]

entry pf (x: i32) : i32 = x ** 2
entry rf (x: r) : r = {x = x.x ** 2, y = x.y + 2}

entry sf (x: s) : s =
  match x
  case #a v -> #c (v + 1)
  case #b v -> #b (v + 2)
  case #c v -> #a (v + 3)

entry ca1 n = map i32.i64 (iota n)

-- The size of the result depends on the input values, so it cannot be known
-- until the entry point has actually run.
entry ca2 (x: []i32) = filter (> 0) x

-- Such a size need not be the outermost one: here the outer dimension is
-- known from the type, but the inner one is not.
entry ca3 (x: []i32) : [2][]i32 = let y = filter (> 0) x in [y, y]

-- Nor need the array be the entire result: it may sit in a tuple or a
-- record, at any depth.
entry ca4 (x: []i32) = (filter (> 0) x, 1i32)
entry ca5 (x: []i32) : {p: []i32, q: i32} = {p = filter (> 0) x, q = 1}
entry ca6 (x: []i32) = ((filter (> 0) x, 2i32), 3i32)

-- An array whose elements are records with a field of unknown size. The
-- elements cannot be inspected to find that size - there may not be any -
-- so the array has to be unzipped instead.
entry ca7 (x: []i32) (n: i64) : [] {a: []i32, b: i32} =
  replicate n {a = filter (> 0) x, b = 1}

-- Entry points that fail at run time, to check how such failures are
-- reported when they happen in compiled code rather than in the interpreter.
entry oob (x: []i32) (i: i64) : i32 = x[i]
entry positive (x: i32) : i32 = assert (x > 0) x

entry pa1f (x: []i32) : []i32 = map (** 2) x
entry pa2f (x: [][]i32) : [][]i32 = let v = map2 (**) x[0, :] x[1, :] in [v, v]

entry ra1f (x: []r) : []r = map rf x
entry ra2f (x: [][]r) : [][]r = map (map rf) x

entry sa1f (x: []s) : []s = map sf x
entry sa2f (x: [][]s) : [][]s = map (map sf) x
