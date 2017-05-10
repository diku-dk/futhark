import "/futlib/array"

let main (length: i32): []i32 =
  let temp = replicate length 1i32
  let with_hoistable_mem = copy temp
  in with_hoistable_mem
