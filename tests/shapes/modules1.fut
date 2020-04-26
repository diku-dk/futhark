-- It is not allowed to create an opaque type whose size parameters
-- are not used in array dimensions.
-- ==
-- error: "n"

module m = {
  type^ t [n] = [n]i32 -> i32
  let f [n] (_: t [n]) = 0
  let mk (n: i32) : t [n] = \(xs: [n]i32) -> n
} : {
  type^ t [n]
  val f [n] : (x: t [n]) -> i32
  val mk : (n: i32) -> t [n]
}

let main x = (x+2) |> m.mk |> m.f
