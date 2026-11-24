def segop 't op (v1: t, f1: bool) (v2: t, f2: bool) : (t, bool) =
  (if f2 then v2 else v1 `op` v2, f1 || f2)

def segreduce [n] 't (op: t -> t -> t) (ne: t) (arr: [n](t, bool)) : *[]t =
  let (res, flags) = unzip (scan (segop op) (ne, false) arr)
  let flagi = map i64.bool flags
  let cnts = scan (+) 0 flagi
  let cnt = if n > 0 then cnts[n - 1] else 0
  let idxs = map2 (\cnt f -> cnt * f - 1) cnts flagi
  in scatter (#[scratch] replicate cnt ne) idxs res

entry main inp =
  segreduce (i32.+) 0 inp
