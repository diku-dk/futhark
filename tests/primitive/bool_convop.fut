-- Convert booleans to different types.
-- ==
-- entry: castB
-- input { [false, true] }
-- output { [false, true] }

-- ==
-- entry: castI8
-- input { [false, true] }
-- output { [0i8, 1i8] }

-- ==
-- entry: castI16
-- input { [false, true] }
-- output { [0i16, 1i16] }

-- ==
-- entry: castI32
-- input { [false, true] }
-- output { [0i32, 1i32] }

-- ==
-- entry: castI64
-- input { [false, true] }
-- output { [0i64, 1i64] }

-- ==
-- entry: castF16
-- input { [false, true] }
-- output { [0f16, 1f16] }

-- ==
-- entry: castF32
-- input { [false, true] }
-- output { [0f32, 1f32] }

-- ==
-- entry: castF64
-- input { [false, true] }
-- output { [0f64, 1f64] }

entry castB = map bool.bool
entry castI8 = map i8.bool
entry castI16 = map i16.bool
entry castI32 = map i32.bool
entry castI64 = map i64.bool
entry castF16 = map f16.bool
entry castF32 = map f32.bool
entry castF64 = map f64.bool
