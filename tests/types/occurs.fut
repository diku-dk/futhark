-- Simple instance of an occurs check.
-- ==
-- error: Occurs

let bad a f = f a f
