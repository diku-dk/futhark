-- ==
-- error: aliased to some other tuple component.

let dup x = (x,x)

let main (xs: []i32) : (*[]i32, *[]i32) =
  dup (copy xs)
