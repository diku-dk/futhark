-- Polymorphic function called incorrectly.
-- ==
-- error: bool.*i32

let f 't (x: t) (y: t) = (x,y)

let main () = f 1 false
