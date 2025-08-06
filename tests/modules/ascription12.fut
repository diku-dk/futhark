module type sized = {
  val len : i64
}

module arr (S: sized) : {type t = [S.len]i32} = {
  type t = [S.len]i32
}

module nine = {def len = 9i64}

module arr_nine : {type t = [nine.len]i32} = arr nine
