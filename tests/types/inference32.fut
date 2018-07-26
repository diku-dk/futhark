-- Inferring a unique type is never allowed - it must always be put
-- there explicitly!
-- ==
-- error: passed non-unique argument

let consume (xs: *[]i32) = xs

let main (xs: []i32) = (\xs -> consume xs) xs
