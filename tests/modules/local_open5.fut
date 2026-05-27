-- ==
-- input { "12" }

type del 'a = #E a | #Del u8

module type obs_monoid = {
  type t
  type ix
  type obs
  val ne : t
  val op : t -> t -> t
  val gen : ix -> t
  val obs : t -> obs
}

module horner : obs_monoid with t = (i64, i64) with ix = u8 with obs = i64 = {
  type t = (i64, i64)
  type obs = i64
  type ix = u8
  def ne : t = (0, 1)
  def op (x, b1) (y, b2) : t = (x + y, b1 + b2)
  def obs (x: t) : obs = x.0
  def gen (x: ix) : t = (i64.u8 x, 10)
}

module m1 : obs_monoid with t = i64 with ix = i64 with obs = i64 = {
  type t = i64
  type ix = i64
  type obs = i64
  def ne = 0i64
  def op (a: i64) (b: i64) : i64 = a + b
  def gen (x: i64) : i64 = x
  def obs (x: i64) : i64 = x
}

module m2 : obs_monoid with t = i64 with ix = del i64 with obs = i64 = {
  type t = i64
  type ix = del i64
  type obs = i64
  def ne = 0i64
  def op (a: i64) (b: i64) : i64 = a + b
  def gen (x: ix) : t = match x case #E v -> v case #Del _ -> 0
  def obs (x: i64) : i64 = x
}

module mk_chunk
  (M1: obs_monoid)
  (M2: obs_monoid with ix = del M1.obs)
  : obs_monoid with ix = del M1.ix with obs = M2.obs = {
  type t = M1.t
  def ne = M1.ne
  def convert (x: M1.t) : M2.t = M2.gen (#E (M1.obs x))
  def op (x: t) (y: t) : t = M1.op x y
  type ix = del M1.ix
  def gen (ix: ix) : t = match ix case #E x -> M1.gen x case #Del _ -> M1.ne
  type obs = M2.obs
  def obs (x: t) : obs = M2.((op (convert x) ne) |> obs)
}

module m5 = mk_chunk m1 m2
module top = mk_chunk horner m5

entry main [n] (xs: [n]u8) : i64 =
  let gen (c: u8): top.t = top.gen (#E c)
  in reduce top.op top.ne (map gen xs) |> top.obs
