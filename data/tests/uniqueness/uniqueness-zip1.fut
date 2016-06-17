-- Ensure that zipping/unzipping does not remove uniqueness, even for
-- arrays of tuples.
fun (*[n](i32,f32),
     *[n](f32,i32))
  main(*[n](i32,f32) array0,
       *[n](f32,i32) array1) =
  let arrays = zip(array0, array1)
  in unzip(arrays)
