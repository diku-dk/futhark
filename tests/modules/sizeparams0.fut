-- A module with abstract types containing size parameters.
-- ==
-- input { 3 } output { 3 }

module type MT = {
  type intvec [n]

  val singleton : i32 -> intvec [1]
  val first [n] : intvec [n] -> i32
}

module M0 : MT = {
  type intvec [n] = [n]i32
  def singleton (x: i32) = [x]
  def first [n] (x: intvec [n]) = x[0]
}

def main (x: i32) : i32 =
  let y: M0.intvec [1] = M0.singleton x
  in M0.first y
