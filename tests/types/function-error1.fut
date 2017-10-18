-- All type parameters must be used in a function parameter.
-- ==
-- error: quux

let f 'quux (x: i32) = x
