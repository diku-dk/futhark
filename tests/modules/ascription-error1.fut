-- We may not access structure members not part of the signature.
-- ==
-- error: Struct.g

module type SIG = {
  val f: i32 -> i32
}

module Struct: SIG = {
  let f (x: i32): i32 = x + 2
  let g (x: i32): i32 = x + 3
}

let main(x: i32): i32 = Struct.g x
