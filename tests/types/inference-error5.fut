-- A type parameter cannot be inferred as a specific type.
-- ==
-- error: Type mismatch

let f 't (x: i32): t = x
