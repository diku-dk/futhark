-- Ensure that zipping/unzipping does not remove uniqueness.
fun main(array0: *[n]i32,
       array1: *[n]i32): (*[n]i32,
     *[n]i32) =
  let arrays = zip(array0, array1)
  in unzip(arrays)
