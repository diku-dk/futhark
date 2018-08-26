let main (a: *[8]f32) : *[8]f32 =
  a with [:] <- copy a
