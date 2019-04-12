module type sized = {
  val len: i32
}

module arr (S: sized): { type t = [S.len]i32 } = {
    type t = [S.len]i32
}

module nine = { let len = 9i32 }

module arr_nine : { type t = [nine.len]i32 } = arr nine
