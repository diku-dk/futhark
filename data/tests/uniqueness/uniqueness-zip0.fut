-- Ensure that zipping/unzipping does not remove uniqueness.
fun (*[i32, n],
     *[i32, n])
  main(*[i32, n] array0,
       *[i32, n] array1) =
  let arrays = zip(array0, array1)
  in unzip(arrays)
