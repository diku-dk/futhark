include futlib.complex

module C = Complex(F32)

-- ==
-- entry: test_sqrt
-- input { 2f32 3f32 }
-- output { 1.674149f32 0.895977f32 }

entry test_sqrt(a: f32, b: f32) =
  let x = C.sqrt (C.mk a b)
  in (C.re x, C.im x)

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
