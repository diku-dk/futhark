-- Minimal reproduction of interpreter bug: when a functor is applied
-- twice (wrapping its own result), the interpreter loses type information
-- in the inner function closures, treating i64 as i32.
--
-- The compiled backend correctly produces ((5i32, 7i64), 9i64),
-- but the interpreter incorrectly produces ((5i32, 7i32), 9i32).

module type monoid = { type t val op : t -> t -> t }

module mk_wrap (M : monoid) : monoid with t = (M.t, i64) = {
  type t = (M.t, i64)
  def op (x: t) (y: t) : t = (M.op x.0 y.0, x.1 + y.1)
}

module i32_m : monoid with t = i32 = {
  type t = i32
  def op (x: t) (y: t) : t = x + y
}

module w1 = mk_wrap i32_m
module w2 = mk_wrap w1

-- ==
-- entry: main
-- input { 1i32 2i64 3i64 4i32 5i64 6i64 }
-- output { 5i32 7i64 9i64 }
def main (a: i32) (b: i64) (c: i64) (d: i32) (e: i64) (f: i64) =
  let r = w2.op ((a,b),c) ((d,e),f)
  in (r.0.0, r.0.1, r.1)
