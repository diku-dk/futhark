-- If something is put in an array, it cannot later be inferred as a
-- function.
-- ==
-- error: functional

let f x = ([x], x 2)
