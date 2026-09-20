-- An abstract type declared inside a module expression must still have
-- a known liftedness when used outside that module expression.
-- ==
-- input { 2i32 } output { [2i32, 2i32] }

module type mt = {
  type t
  val mk : i32 -> t
  val un : t -> i32
}

module opened = {
  open ({type t = i32 def mk (x: i32) = x def un (x: t) = x}: mt)
}

module nested = {
  module M : mt = {type t = i32 def mk (x: i32) = x def un (x: t) = x}
}

entry main (x: i32) =
  map opened.un [opened.mk x, opened.mk x]
  |> map nested.M.mk
  |> map nested.M.un
