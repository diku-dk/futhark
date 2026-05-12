def splits [n] 'a (p: a -> bool) (s: [n]a) =
  let m = n + 1
  in ( \(_, i, k) -> #[unsafe] s[i:i + k]
     , map (\i -> (replicate m (), i, i + 1)) (indices s)
     )

def main (s: []u8) =
  let (get, fs) = splits (== '-') s
  let on_f (_, i, k) = length s[i:i + k]
  in map on_f fs
