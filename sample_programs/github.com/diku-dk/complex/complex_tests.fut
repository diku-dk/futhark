-- | ignore

import "complex"

module c32 = mk_complex f32

-- ==
-- entry: test_mag
-- input { 2f32 3f32 }
-- output { 3.605551275463989f32 }
-- input { 0f32 0f32 }
-- output { 0f32 }
-- input { -1f32 0f32 }
-- output { 1f32 }
entry test_mag (a: f32) (b: f32) =
  c32.mag (c32.mk a b)

-- ==
-- entry: test_arg
-- input { 2f32 3f32 }
-- output { 0.982793723247329f32 }
-- input { -2f32 3f32 }
-- output { 2.158798930342464f32 }
-- input { -2f32 -3f32 }
-- output { -2.158798930342464f32 }
-- input { 2f32 -3f32 }
-- output { -0.982793723247329f32 }
entry test_arg (a: f32) (b: f32) =
  c32.arg (c32.mk a b)

-- ==
-- entry: test_add
-- input { 2f32 3f32 4f32 5f32 }
-- output { 6f32 8f32 }
entry test_add (a: f32) (b: f32) (c: f32) (d: f32) =
  let x = c32.mk a b
  let y = c32.mk c d
  let z = x c32.+ y
  in (c32.re z, c32.im z)

-- ==
-- entry: test_sub
-- input { 2f32 5f32 4f32 3f32 }
-- output { -2f32 2f32 }
entry test_sub (a: f32) (b: f32) (c: f32) (d: f32) =
  let x = c32.mk a b
  let y = c32.mk c d
  let z = x c32.- y
  in (c32.re z, c32.im z)

-- ==
-- entry: test_mul
-- input { 3f32 2f32 1f32 4f32}
-- output { -5f32 14f32 }
entry test_mul (a: f32) (b: f32) (c: f32) (d: f32) =
  let x = c32.mk a b
  let y = c32.mk c d
  let z = x c32.* y
  in (c32.re z, c32.im z)

-- ==
-- entry: test_div
-- input { 3f32 2f32 1f32 4f32}
-- output { 0.647058824f32 -0.588235294f32 }
entry test_div (a: f32) (b: f32) (c: f32) (d: f32) =
  let x = c32.mk a b
  let y = c32.mk c d
  let z = x c32./ y
  in (c32.re z, c32.im z)

-- ==
-- entry: test_sqrt
-- input { 2f32 3f32 }
-- output { 1.674149f32 0.895977f32 }
entry test_sqrt (a: f32) (b: f32) =
  let x = c32.sqrt (c32.mk a b)
  in (c32.re x, c32.im x)

-- ==
-- entry: test_exp
-- input { 2f32 3f32 }
-- output { -7.315110094901103f32 1.0427436562359045f32 }
-- input { 0f32 0f32 }
-- output { 1f32 0f32 }
entry test_exp (a: f32) (b: f32) =
  let x = c32.exp (c32.mk a b)
  in (c32.re x, c32.im x)

-- ==
-- entry: test_log
-- input { 1f32 0f32 }
-- output { 0f32 0f32 }
-- input { 2f32 3f32 }
-- output { 1.2824746787307684f32 0.982793723247329f32 }
entry test_log (a: f32) (b: f32) =
  let x = c32.log (c32.mk a b)
  in (c32.re x, c32.im x)

-- ==
-- entry: test_conj
-- input { 2f32 3f32 }
-- output { 2f32 -3f32 }
entry test_conj (a: f32) (b: f32) =
  let x = c32.conj (c32.mk a b)
  in (c32.re x, c32.im x)
