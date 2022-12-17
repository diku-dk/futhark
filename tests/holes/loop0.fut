def main [j] (jets: [j]u8) =
  let h = 20
  let world = replicate h (replicate 7 0)
  let skipped = 0
  let top = i64.i64 (length world)
  let jet_num = 0
  let rock_num = 0
  let target = 10
  let (_, skipped, top, _, _) =
    loop (jet_num, skipped, top, world, rock_num) while rock_num < target do
      if ???
      then ???
      else (jet_num, skipped, top, world, rock_num)
  in skipped + top
