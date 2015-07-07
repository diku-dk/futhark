-- Test that we can handle a consuming map where only part of the
-- input is being consumed.  Does not compute anything interesting,
-- but may confuse the type checker.
-- ==
fun *[[real]] main( [[real]] a, *[[real]] b) =
  map(fn *[real] ( {[real],*[real]} tup )  =>
	let {a_row, b_row} = tup in
	b_row,
      zip(a,b))
