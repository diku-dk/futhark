-- Ensure that zipping/unzipping does not remove uniqueness, even for
-- arrays of tuples.
fun main(array0: *[n]i32, array1: *[n]f32, array2: *[n]i32, array3: *[n]f32): (*[n]i32, *[n]f32, *[n]i32, *[n]f32) =
  let arrays = zip(zip(array0, array1), zip(array2, array3))
  let (arrays01, arrays23) = unzip(arrays)
  let (arrays0', arrays1') = unzip(arrays01)
  let (arrays2', arrays3') = unzip(arrays23)
  in (arrays0', arrays1', arrays2', arrays3')
