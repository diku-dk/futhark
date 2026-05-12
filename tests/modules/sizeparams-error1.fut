-- A dimension parameter using a name bound in the module type.
-- ==
-- error: k_ints

type ints [n] = [n]i32

module type MT = {
  val k : i64
  type k_ints = ints [k]
}

module M_k2 : MT = {
  def k = 2i64
  type k_ints = ints [2]
}

def main (n: i32) : M_k2.k_ints = iota n
