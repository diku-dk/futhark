-- This test exposed a bug in the index functions of group stream
-- lambda parameter index functions.  The index function of the split
-- would be overwritten by the new offset of the group lambda chunk.
--
-- Thus, not technically a split feature, but where else to put it?
--
-- ==
-- input { 10 }
-- output { [4i32, 3i32, 4i32, 2i32, 4i32, 2i32, 3i32, 2i32, 2i32, 1i32] }

let boolToInt (x: bool): i32 =
  if x
  then 1
  else 0

let resi (x: i32) (y: i32): i32 =
  if (x == 0)
  then y
  else (y % x)

entry main (n: i32): []i32 =
  let (_, t_v1) = split 1 (iota (n+1)) in
  let t_v7 = transpose (replicate n (t_v1 :> [n]i32)) in
  let t_v8 = unflatten n n (iota (n*n)) in
  let t_v12 = let (array: [][n]i32) = map2 (\(x: []i32) (y: []i32): [n]i32 ->
                                              map2 resi (x) (y)) t_v7 t_v8 in
              map (\(x: []i32): [n]bool ->
                   map (0==) x) (array) in
  let array =
    (map (\(x: []i32): i32 -> reduce (+) (0) (x))
     (let (array: [][n]bool) = transpose (t_v12) in
      map (\(x: []bool): [n]i32 ->
             map boolToInt (x)) (array)))
  in array
