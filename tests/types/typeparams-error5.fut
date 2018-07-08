-- Cannot create an array containing elements of a lifted type parameter.
-- ==
-- error: Cannot create array

type t '^a = []a
