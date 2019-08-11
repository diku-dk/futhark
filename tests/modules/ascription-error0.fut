-- Abstract types must be abstract.
-- ==
-- error: Types do not match

module type SIG = {
type t

val inject: i32 -> t
val extract: t -> i32
}

module Struct: SIG = {
type t = i32

let inject (x: i32): i32 = x
let extract (x: i32): i32 = x
}

let main(x: i32): i32 =
  Struct.inject x
