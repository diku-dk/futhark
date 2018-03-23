-- We should not be able to instantiate a value type parameter of a
-- polymorphic function with a function type.
-- ==
-- error: functional

let mkArray 'a (x : a) : []a = [x]

let main (x : i32) =
  let _ = mkArray (\(x:i32) -> x)
  in x
