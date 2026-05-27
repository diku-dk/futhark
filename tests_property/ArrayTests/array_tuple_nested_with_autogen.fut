let all_equal (r: ([](i8, i16), i32)) : bool =
  let (pairs, target) = r
  in all (\(a, b) -> i32.i8 a == target && i32.i16 b == target) pairs

let prop_all_equal (xs: []([](i8, i16), i32)) : bool =
  map all_equal xs |> reduce (&&) true

-- ==
-- property: prop_record_sums_fail
#[prop(size(5))]
entry prop_record_sums_fail (input: []([](i8, i16), i32)) : bool =
  if input[0].0[0].0 <= 0i8 && input[0].0[0].1 >= 0i16 && input[0].1 >= 0i32 then
    false
  else
    true
