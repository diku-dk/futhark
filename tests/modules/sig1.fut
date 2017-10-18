-- Signature with abstract type.

module type MONOID = {
  type t

  val neutral: t
  val op: t -> t -> t
}

let main(): i32 = 0
