import "/futlib/math"
import "/futlib/complex"

module C = complex(f32)

-- ==
-- entry: test_mag
-- input { 2f32 3f32 }
-- output { 3.605551275463989f32 }
-- input { 0f32 0f32 }
-- output { 0f32 }
-- input { -1f32 0f32 }
-- output { 1f32 }
entry test_mag(a: f32, b: f32) =
  C.mag (C.mk a b)

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
entry test_arg(a: f32, b: f32) =
  C.arg (C.mk a b)

-- ==
-- entry: test_add
-- input { 2f32 3f32 4f32 5f32 }
-- output { 6f32 8f32 }
entry test_add(a: f32, b: f32, c: f32, d: f32) =
  let x = C.mk a b
  let y = C.mk c d
  let z = x C.+ y
  in (C.re z, C.im z)

-- ==
-- entry: test_sub
-- input { 2f32 5f32 4f32 3f32 }
-- output { -2f32 2f32 }
entry test_sub(a: f32, b: f32, c: f32, d: f32) =
  let x = C.mk a b
  let y = C.mk c d
  let z = x C.- y
  in (C.re z, C.im z)

-- ==
-- entry: test_mul
-- input { 3f32 2f32 1f32 4f32}
-- output { -5f32 14f32 }
entry test_mul(a: f32, b: f32, c: f32, d: f32) =
  let x = C.mk a b
  let y = C.mk c d
  let z = x C.* y
  in (C.re z, C.im z)

-- ==
-- entry: test_div
-- input { 3f32 2f32 1f32 4f32}
-- output { 0.647058824f32 -0.588235294f32 }
entry test_div(a: f32, b: f32, c: f32, d: f32) =
  let x = C.mk a b
  let y = C.mk c d
  let z = x C./ y
  in (C.re z, C.im z)

-- ==
-- entry: test_sqrt
-- input { 2f32 3f32 }
-- output { 1.674149f32 0.895977f32 }
entry test_sqrt(a: f32, b: f32) =
  let x = C.sqrt (C.mk a b)
  in (C.re x, C.im x)

-- ==
-- entry: test_exp
-- input { 2f32 3f32 }
-- output { -7.315110094901103f32 1.0427436562359045f32 }
-- input { 0f32 0f32 }
-- output { 1f32 0f32 }
entry test_exp(a: f32, b: f32) =
  let x = C.exp (C.mk a b)
  in (C.re x, C.im x)

-- ==
-- entry: test_log
-- input { 1f32 0f32 }
-- output { 0f32 0f32 }
-- input { 2f32 3f32 }
-- output { 1.2824746787307684f32 0.982793723247329f32 }
entry test_log(a: f32, b: f32) =
  let x = C.log (C.mk a b)
  in (C.re x, C.im x)

-- ==
-- entry: test_conj
-- input { 2f32 3f32 }
-- output { 2f32 -3f32 }
entry test_conj(a: f32, b: f32) =
  let x = C.conj (C.mk a b)
  in (C.re x, C.im x)
