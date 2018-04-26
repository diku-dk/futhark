-- Polymorphic function called incorrectly.
-- ==
-- error: Cannot unify

let f 't (x: t) (y: t) = (x,y)

let main () = f 1 false
