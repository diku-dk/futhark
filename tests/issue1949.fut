def fn (arr: *[](i32, i32)) =
  if true then opaque arr else opaque arr

entry test = (fn [(0, 0)])[0]
