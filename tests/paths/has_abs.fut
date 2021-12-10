-- File containing an abstract type.

module m = {
  type t = i32
  def x = 0i32
  def eq = (i32.==)
}

open (m : {
  type t
  val x: t
  val eq: t -> t -> bool
})
