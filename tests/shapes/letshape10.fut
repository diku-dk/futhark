-- ==
-- input {true} output { 1i64 }

module type mt = {
  type arr [n]
  val mk : bool -> arr []
}

module m : mt = {
  type arr [n] = [n]bool
  let mk b = [b]
}

let main b =
  let [n] (_: m.arr [n]) = m.mk b
  in n
