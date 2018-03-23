-- If any type parameters are given, they must fully describe any type
-- variables in the result of a function.
-- ==
-- error: not closed over

let id 't x = x
