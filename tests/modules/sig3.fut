-- ==
-- input { 2 true }
-- output { [true,true] }

module type mt = {
  val replicate 't: (n: i32) -> t -> [n]t
}

module m: mt = {
  let replicate 't (n: i32) (x: t): [n]t =
    map (\_ -> x) (iota n)
}

let main (n: i32) (x: bool) = m.replicate n x