-- Ensure that zipping/unzipping does not remove uniqueness, even for
-- arrays of tuples.
fun (*[(i32,f32), n],
     *[(f32,i32), n])
  main(*[(i32,f32), n] array0,
       *[(f32,i32), n] array1) =
  let arrays = zip(array0, array1)
  in unzip(arrays)
