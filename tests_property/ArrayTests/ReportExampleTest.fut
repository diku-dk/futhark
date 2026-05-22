entry gen_i32_array (size: i64) (seed: u64) : []i32 =
  let n = i64.max 0 size
  in tabulate n (\i ->
                   let x = seed + i32.i64 i
                   in (x * 1103515245i32 + 12345i32) % (i32.i64 (size + 1)))

entry shrink_i32_array (xs: []i32) (random: u64) : []i32 =
  let n = length xs
  in if n == 0
     then xs
     else if random % 2i32 == 0i32
     then -- Try shrinking the structure by removing the last element.
          take (n - 1) xs
     else -- Try shrinking the values by moving every element toward zero.
          map (\x -> x / 2i32) xs

-- ==
-- entry: prop_reverse_involution
-- input { [1i32, 1i32, 1i32] }
-- output { true }

-- ==
-- property: prop_reverse_involution

#[prop(gen(gen_i32_array),shrink(shrink_i32_array))]
entry prop_reverse_involution (xs: []i32) : bool =
  and (map2 (==) ((reverse xs)) xs)
