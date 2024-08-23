module m : {
  type t
  val mk : i32 -> [2]f32 -> t
  val unmk : t -> (i32,[2]f32)
} = {
  type t = #foo (i32, [2]f32)
  def mk x y : t = #foo (x,y)
  def unmk (x: t) = match x case #foo (x,y) -> (x,y)
}

type t = m.t

entry mk (x: i32) (y: [2]f32) : t = m.mk x y
entry unmk (x: t): (i32, [2]f32) = m.unmk x
entry arr (x: t) : [2]t = [x, x]
