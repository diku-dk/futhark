-- ==
-- entry: atanpi64
-- input { [0f64, 1.633123935319537e16f64, 5.443746451065123e15f64] }
-- output { [0f64, 0.5f64, 0.5f64 ] }

-- ==
-- entry: atanpi32
-- input { [0f32, -2.2877334e7f32, -8.385828e7f32] }
-- output { [0f32, -0.49999997f32, -0.49999997f32 ] }

-- ==
-- entry: atanpi16
-- input { [0f16, 2066f16, 689f16 ] }
-- output { [0f16, 0.5f16, 0.5f16 ] }

entry atanpi64 = map f64.atanpi
entry atanpi32 = map f32.atanpi
entry atanpi16 = map f16.atanpi
