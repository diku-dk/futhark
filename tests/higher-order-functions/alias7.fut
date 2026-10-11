-- A lambda may return a global variable, as long as it does not escape
-- the enclosing named function.  Here the lambda is consumed by
-- "tabulate", whose return type is unique, so the array that "f"
-- returns is freshly constructed and aliases nothing.
-- ==
-- input { 3i64 }
-- output { [[999i64,2i64,3i64],[1i64,2i64,3i64],[1i64,2i64,3i64]] [1i64,2i64,3i64] }

module type monoid = {
  type t
  val ne : t
  val op : t -> t -> *t
}

module mk_sum (M: monoid) = {
  def f n = tabulate n (\_ -> M.ne)
}

module vecs = {
  type t = [3]i64
  def ne : [3]i64 = [1, 2, 3]
  def op (a: [3]i64) (b: [3]i64) : *[3]i64 = map2 (+) a b
}

module sums = mk_sum vecs

def main (n: i64) =
  let arr = sums.f n
  let arr' = arr with [0, 0] = 999
  in (arr', copy vecs.ne)
