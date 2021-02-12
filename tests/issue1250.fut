-- ==
-- input { }

module type mt = {
  type t
  val to_i64 : t -> i64
}

module i8mt = {
  type t = i8
  let to_i64 = i8.to_i64
}

module type a = {
    module b: mt
    module c: mt
}

module a_impl = {
    module b = i8mt
    module c = i8mt
}

module use_a (d: a) = {
    let b_to_i64 (b: d.b.t) = d.b.to_i64 b
}

module f = use_a a_impl

let main = f.b_to_i64 10
