-- File containing an abstract type.

module m = {
  type t = i32
  let x = 0i32
  let eq = (i32.==)
}

open (m : {
  type t
  val x: t
  val eq: t -> t -> bool
})
