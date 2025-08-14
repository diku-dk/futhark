-- Check that value mismatches in nested modules use qualified names.
-- ==
-- error: bar.x

module type mt = {
  module foo: {
    val x : i32
  }

  module bar: {
    val x : bool
  }
}

module m : mt = {
  module foo = {def x : i32 = 1}
  module bar = {def x : i32 = 1}
}
