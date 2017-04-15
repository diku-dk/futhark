-- A dimension parameter using a name bound in the module type.
-- ==
-- error: k_ints

type ints [n] = [n]i32

module type MT = {
  val k: i32
  type k_ints = ints [k]
}

module M_k2: MT = {
  let k = 2
  type k_ints = ints [2]
}

let main(n: i32): M_k2.k_ints = iota n
