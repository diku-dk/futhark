-- A module spec in a module type, used for a parametric module, with
-- some shadowing too.
-- ==
-- input { 10 } output { 10 }

module PM(P: {type t val x: t module PM: {val f: t -> t}}) = {
  let iterate(n: i32) = loop x = copy P.x for i < n do P.PM.f x
}

module M = PM({
  type t = i32
  let x = 0
  module PM = {
    let f(a: i32) = a + 1
  }
})

let main(n: i32) = M.iterate n
