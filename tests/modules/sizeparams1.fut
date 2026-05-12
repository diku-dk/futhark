-- A dimension parameter using a name bound in the module type.
-- ==
-- input { 2i64 } output { [0i64,1i64] }
-- input { 1i64 } error:

type ints [n] = [n]i64

module type MT = {
  val k : i64
  type k_ints = ints [k]
}

module M_k2 : MT = {
  def k = 2i64
  type k_ints = ints [k]
}

def main (n: i64) = iota n :> M_k2.k_ints
