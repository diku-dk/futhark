let all_equal (r: ([](i8, i16), i32)) : bool =
  let (pairs, target) = r
  in all (\(a, b) -> i32.i8 a == target && i32.i16 b == target) pairs

let prop_all_equal (xs: []([](i8, i16), i32)) : bool =
  map all_equal xs |> reduce (&&) true

-- | Generator needs to return the specific nested structure expected by the property
entry gen_record_sums_fail (size: i64) (_seed: u64) : []([](i8, i16), i32) =
  let n = if size < 0 then 0 else size
  in tabulate n (\_ ->([(1i8, 10i16)], 100i32))

-- ==
-- property: prop_record_sums_fail
#[prop(gen(gen_record_sums_fail),  size(5))]
entry prop_record_sums_fail (input: []([](i8, i16), i32)) : bool =
  if input[0] == ([(1i8, 10i16)], 100i32) then
    false
  else
    true
