-- A dimension parameter using a name bound in the module type.
-- ==
-- input { 2 } output { [0,1] }
-- input { 1 } error:

type ints [n] = [n]i32

module type MT = {
  val k: i32
  type k_ints = ints [k]
}

module M_k2: MT = {
  let k = 2
  type k_ints = ints [k]
}

let main (n: i32) = iota n :> M_k2.k_ints
