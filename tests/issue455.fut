def main (data: *[]i32) : []i32 =
  let old_data = copy data
  let (data, _) =
    loop (data, old_data) for i in [1, 2, 3] do
      let new_data = old_data with [0] = 1
      in (new_data, data)
  in data
