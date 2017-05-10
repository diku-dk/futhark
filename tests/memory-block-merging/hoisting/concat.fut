import "/futlib/array"

let main (length0: i32, length1: i32): []i32 =
  let temp0 = replicate length0 1i32
  let temp1 = replicate length1 1i32
  let with_hoistable_mem = concat temp0 temp1
  in with_hoistable_mem
