def dupcopy (xs: []i32) : (*[]i32, *[]i32) = (copy xs, copy xs)

def main xs : (*[]i32, *[]i32) = dupcopy xs
