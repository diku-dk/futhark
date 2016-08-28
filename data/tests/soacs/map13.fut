-- Test that we can handle a consuming map where only part of the
-- input is being consumed.  Does not compute anything interesting,
-- but may confuse the type checker.
-- ==
fun main(a:  [][]f64, b: *[][]f64): *[][]f64 =
  map (fn (tup:  ([]f64,*[]f64) ): *[]f64   =>
	let (a_row, b_row) = tup in
	b_row) (
      zip(a,b))
