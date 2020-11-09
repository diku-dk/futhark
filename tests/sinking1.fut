-- ==
-- structure distributed { /SegMap/Index 1 }

let main (as: []i32) (bs: []i32) (cs: []i32) (ds: []i32) (es: []i32) =
  map5 (\a b c d e -> if a == 0 then 0 else b + c + d + e) as bs cs ds es
