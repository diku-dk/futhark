-- Abstract types must be abstract.
-- ==
-- error: Function body does not have

module type SIG = {
  type t

  val inject : i32 -> t
  val extract : t -> i32
}

module Struct : SIG = {
  type t = i32

  def inject (x: i32) : i32 = x
  def extract (x: i32) : i32 = x
}

def main (x: i32) : i32 =
  Struct.inject x
