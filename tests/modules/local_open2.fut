-- Local open that involves values of an abstract type.
-- ==

module type has_t = {
  type t
  val f: i32 -> t
}

module pm (num: has_t) = {
  let test (x: i32) =
    num.(f x)
}

module m = pm {
  type t = i32
  let f (x: i32) = x
}

let main (x: i32) = m.test x
