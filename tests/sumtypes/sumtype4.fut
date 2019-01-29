-- Constructors with different fields should be different.
-- ==
-- error:

let g (x : #foo i32) : #foo i16 = 
  match x
    case y -> y