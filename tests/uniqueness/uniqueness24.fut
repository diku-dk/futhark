let main (arr: *[]i32) =
  let a = arr[0]
  let arr' = rotate 1 arr
  let arr'[0] = 0
  let arr'[1] = a
  in arr'
