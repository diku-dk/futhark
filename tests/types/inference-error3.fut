-- If something is applied, it cannot later be put in an array.
-- ==
-- error: -> b

let f x = (x 2, [x])
