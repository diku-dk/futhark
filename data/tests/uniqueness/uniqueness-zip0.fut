-- Ensure that zipping/unzipping does not remove uniqueness.
fun (*[n]i32,
     *[n]i32)
  main(*[n]i32 array0,
       *[n]i32 array1) =
  let arrays = zip(array0, array1)
  in unzip(arrays)
