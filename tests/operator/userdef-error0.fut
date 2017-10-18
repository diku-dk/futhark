-- You can't override &&.
-- ==
-- error: &&

let (x: bool) && (y: bool) = x

let main(x: bool) = x && x
