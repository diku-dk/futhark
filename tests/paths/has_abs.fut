-- File containing an abstract type.

open (
  {
    type t = i32
    def x = 0i32
    def eq = (i32.==)
  }:
  {
    type t
    val x : t
    val eq : t -> t -> bool
  })
