// Test that a simple consuming map works.
fun [[real]] main(*[[real]] a) =
  map(fn *[real] (*[real] r) =>
        r,
      a)
