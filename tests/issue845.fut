module type mt = {
  type~ t
  val mk : i32 -> *t
  val f : *t -> *t
}

module m : mt = {
  type~ t = []i32
  let mk (x: i32) = [x]
  let f (xs: *[]i32) = xs with [0] = xs[0] + 1
}

let main (x: i32) =
  let f = \(xs: *m.t) -> m.f xs
  in f (m.mk x)
