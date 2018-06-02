-- Assertion condition must be a boolean.
-- ==
-- error: bool

let main (x: i32) = assert 0 (x/2)
