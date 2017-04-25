-- Entry points may not be polymorphic.
-- ==
-- error: polymorphic

let main 't (x: t) = x
