-- Polymorphic function called incorrectly.
-- ==
-- error: Couldn't match

let f 't (x: t) (y: t) = (x,y)

let main () = f 1 false
