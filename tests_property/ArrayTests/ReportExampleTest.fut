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

def digit_to_u8 (d: u64) : u8 =
  '0' + u8.u64 d

def pos_u64_to_string (x: u64) : []u8 =
  if x == 0u64
  then "0"
  else let (_, acc) =
         loop (n, acc) = (x, "")
         while n > 0u64 do
           let digit = n % 10u64
           let ch = digit_to_u8 digit
           in (n / 10u64, [ch] ++ acc)
       in acc

def i32_to_string (x: i32) : []u8 =
  if x >= 0i32
  then pos_u64_to_string (u64.i32 x)
  else if x == -2147483648i32
  then "-2147483648"
  else "-" ++ pos_u64_to_string (u64.i32 (-x))

entry print_i32_array (xs: []i32) : []u8 =
  let n = length xs
  let body =
    loop acc = ""
    for i < n do
      let sep = if i == 0 then "" else ", "
      in acc ++ sep ++ i32_to_string xs[i]
  in "[" ++ body ++ "]"

-- ==
-- property: prop_reverse_involution

#[prop(gen(gen_i32_array),shrink(shrink_i32_array),pprint(print_i32_array))]
entry prop_reverse_involution (xs: []i32) : bool =
  and (map2 (==) ((reverse xs)) xs)
