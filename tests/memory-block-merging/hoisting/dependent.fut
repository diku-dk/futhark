import "/futlib/array"

let main (length: i32): []i32 =
  let temp = replicate length 1i32
  let with_hoistable_mem0 = copy temp
  let with_hoistable_mem1 = concat temp with_hoistable_mem0
  in with_hoistable_mem1
