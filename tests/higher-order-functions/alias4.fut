-- A partially applied function that closes over a value, but whose
-- return type is unique, must not make the result of applying it
-- (here through "|>") alias that value.
-- ==
-- input { [1i64,2i64,3i64,4i64] } output { 10i64 10i64 }

module type monoid = {
  type t
  val ne : t
  val op : t -> t -> *t
}

module mk_sum (M: monoid) = {
  def sum (xs: []M.t) = xs |> reduce M.op M.ne
  def sum_l (xs: []M.t) = reduce M.op M.ne <| xs
}

module sum_i64 = mk_sum {
  type t = i64
  def ne = 0i64
  def op (x: i64) (y: i64) = x + y
}

def main (xs: []i64) = (sum_i64.sum xs, sum_i64.sum_l xs)
