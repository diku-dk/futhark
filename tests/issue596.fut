-- ==
-- error: passed non-unique

let consume (xs: *[]i32) = xs

let main (xss: [][]i32) = map (\xs -> consume xs) xss
