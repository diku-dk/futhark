-- ==
-- property: prop_record_sums_succ

-- ==
-- property: prop_record_sums_fail

type~ record = { s: []i32, a: []i32 }

let all_equal (xs: []i32) : bool =
  let n = length xs
  in if n == 0 then true
     else
       let first = xs[0]
       in map (\x -> x == first) xs
          |> reduce (&&) true

entry gen_record_sums (size: i64) (seed: i32) : record =
  let s = replicate size (i32.i64 size)
  let a = replicate size seed
  in { s, a }

#[prop(gen(gen_record_sums))]
entry prop_record_sums_succ (input: record) : bool =
  all_equal input.s && all_equal input.a


entry gen_record_sums_fail (size: i64) (seed: i32) : record =
  let s = map2 (\v i -> if i == 0 then 0 else v) (replicate size (i32.i64 size)) (iota size) 
  let a = replicate size seed
  in { s, a }

#[prop(gen(gen_record_sums_fail))]
entry prop_record_sums_fail (input: record) : bool =
  all_equal input.s && all_equal input.a
