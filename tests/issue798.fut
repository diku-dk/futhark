-- Variables should not be in scope in their own type.

module m = {
  type t = i32
}

let main (m: m.t) = m
