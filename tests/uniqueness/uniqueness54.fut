let dupcopy (xs: []i32) : (*[]i32, *[]i32) = (copy xs, copy xs)

let main xs : (*[]i32, *[]i32) = dupcopy xs
