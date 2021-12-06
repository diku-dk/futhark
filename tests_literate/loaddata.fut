-- > $loaddata "data/array.in"

let add_scalar (y: i32) = map (+y)

-- > let (xs, y) = $loaddata "data/array_and_value.in" in add_scalar y xs
