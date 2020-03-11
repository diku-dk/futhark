-- No switcharoos.
-- ==
-- error: Function body does not have expected type

let id 'a 'b (x: a) (y: b): (a, b) = (y, x)
