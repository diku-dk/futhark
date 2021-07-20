-- =
-- entry: update_antidiag
-- script input { iota 100 }
-- output { [ 0i64,  1i64,  2i64,  3i64,  4i64,  5i64,  6i64,  7i64,  8i64,  9i64,
--           10i64, 11i64, 12i64, 13i64, 14i64, 15i64, 16i64,  0i64,  1i64,  2i64,
--           20i64, 21i64, 22i64, 23i64, 24i64, 25i64, 26i64,  3i64,  4i64,  5i64,
--           30i64, 31i64, 32i64, 33i64, 34i64, 35i64, 36i64,  6i64,  7i64,  8i64,
--           40i64, 41i64, 42i64, 43i64,  9i64, 10i64, 11i64, 47i64, 48i64, 49i64,
--           50i64, 51i64, 52i64, 53i64, 12i64, 13i64, 14i64, 57i64, 58i64, 59i64,
--           60i64, 61i64, 62i64, 63i64, 15i64, 16i64, 17i64, 67i64, 68i64, 69i64,
--           70i64, 18i64, 19i64, 20i64, 74i64, 75i64, 76i64, 77i64, 78i64, 79i64,
--           80i64, 21i64, 22i64, 23i64, 84i64, 85i64, 86i64, 87i64, 88i64, 89i64,
--           90i64, 24i64, 25i64, 26i64, 94i64, 95i64, 96i64, 97i64, 98i64, 99i64] }

entry update_antidiag [n] (xs: *[n]i64): [n]i64 =
  let vs = iota (3 * 3 * 3) |> unflatten 9 3 |> unflatten 3 3
  let zs = xs with [17i64; 3i64 : 27i64, 3i64 : 10i64, 3i64: 1i64] = vs
  in zs


-- We need to test weird inner strides as well

-- And I guess negative strides?
