-- ==
-- structure { Screma 2 }

def main [n] (xs: [n]i32) : [n]i32 =
  let num x = x & 1
  let pairwise op (a1, b1) (a2, b2) = (a1 `op` a2, b1 `op` b2)
  let bins = xs |> map num
  let flags = bins |> map (\x -> if x == 0 then (1, 0) else (0, 1))
  let offsets = scan (pairwise (+)) (0, 0) flags
  let f bin (a, b) =
    match bin
    case 0 -> a - 1
    case _ -> (last offsets).0 + b - 1
  let is = map2 f bins offsets
  in scatter (copy xs) is xs
