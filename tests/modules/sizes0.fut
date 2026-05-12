module type sized = {
  val len : i64
}

module arr (S: sized) = {
  type t = [S.len]i32
}

module mt (P: sized)
  : {
      type t = [P.len]i32
    } =
  arr P
