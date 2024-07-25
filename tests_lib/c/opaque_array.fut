module m : {
  type t
  val mk : i32 -> [2]f32 -> t
  val unmk : t -> (i32,[2]f32)
} = {
  type t = #foo (i32, [2]f32)
  def mk x y : t = #foo (x,y)
  def unmk (x: t) = match x case #foo (x,y) -> (x,y)
}

entry mk x y = m.mk x y
entry unmk x = m.unmk x
entry arr x = [x : m.t, x]
