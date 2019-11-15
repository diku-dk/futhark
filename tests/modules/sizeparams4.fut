-- ==
-- input { 2 } output { 2 }

module type mt = {
  type^ abs
  val mk : i32 -> abs
  val len : abs -> i32
}

module m : mt = {
  type~ abs = []i32
  let mk (n: i32) = iota n
  let len [n] (_: [n]i32) = n
}

let main (x: i32) = m.len (m.mk x)
