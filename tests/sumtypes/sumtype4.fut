-- Constructors with different fields should be different.
-- ==
-- error: #foo i16.*#foo i32

let g (x : #foo i32) : #foo i16 =
  match x
    case y -> y
