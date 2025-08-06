-- Signature with abstract type.

module type MONOID = {
  type t

  val neutral : t
  val op : t -> t -> t
}

def main () : i32 = 0
