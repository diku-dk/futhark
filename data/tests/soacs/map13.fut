-- Test that we can handle a consuming map where only part of the
-- input is being consumed.  Does not compute anything interesting,
-- but may confuse the type checker.
-- ==
fun *[[f64]] main( [[f64]] a, *[[f64]] b) =
  map(fn *[f64] ( ([f64],*[f64]) tup )  =>
	let (a_row, b_row) = tup in
	b_row,
      zip(a,b))
